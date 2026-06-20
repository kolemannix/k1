#!/usr/bin/env python3
import argparse
import asyncio
import os
import shlex
import statistics
import subprocess
import sys
import time
from dataclasses import dataclass
from typing import List, Optional, Sequence, Tuple


@dataclass
class Result:
    ok: bool
    status: Optional[int]
    latency_ms: float
    bytes_read: int
    error: Optional[str] = None


async def read_response(reader: asyncio.StreamReader, timeout: float) -> Tuple[int, int]:
    header_bytes = await asyncio.wait_for(reader.readuntil(b"\r\n\r\n"), timeout)
    header_text = header_bytes.decode("iso-8859-1", errors="replace")
    header_lines = header_text.split("\r\n")
    status_parts = header_lines[0].split(" ", 2)
    if len(status_parts) < 2 or not status_parts[1].isdigit():
        raise ValueError(f"bad status line: {header_lines[0]!r}")

    content_length = 0
    for line in header_lines[1:]:
        if line.lower().startswith("content-length:"):
            content_length = int(line.split(":", 1)[1].strip())
            break

    body = await asyncio.wait_for(reader.readexactly(content_length), timeout)
    return int(status_parts[1]), len(header_bytes) + len(body)


def request_bytes(host: str, path: str, request_id: int) -> bytes:
    return (
        f"GET {path} HTTP/1.1\r\n"
        f"Host: {host}\r\n"
        f"User-Agent: k1-load-http/1\r\n"
        f"X-Request-Id: {request_id}\r\n"
        f"Accept: */*\r\n"
        f"\r\n"
    ).encode("ascii")


async def send_one(
    host: str,
    port: int,
    path: str,
    request_id: int,
    timeout: float,
) -> Result:
    started = time.perf_counter()
    writer = None
    try:
        reader, writer = await asyncio.wait_for(asyncio.open_connection(host, port), timeout)
        writer.write(request_bytes(host, path, request_id))
        await asyncio.wait_for(writer.drain(), timeout)
        status, bytes_read = await read_response(reader, timeout)
        latency_ms = (time.perf_counter() - started) * 1000
        return Result(status == 200, status, latency_ms, bytes_read)
    except Exception as exc:
        latency_ms = (time.perf_counter() - started) * 1000
        return Result(False, None, latency_ms, 0, f"{type(exc).__name__}: {exc}")
    finally:
        if writer is not None:
            writer.close()
            try:
                await writer.wait_closed()
            except Exception:
                pass


async def worker(
    worker_id: int,
    queue: asyncio.Queue[int],
    args: argparse.Namespace,
    paths: Sequence[str],
    results: List[Result],
) -> None:
    while True:
        try:
            request_id = queue.get_nowait()
        except asyncio.QueueEmpty:
            return

        path = paths[request_id % len(paths)]
        result = await send_one(args.host, args.port, path, request_id, args.timeout)
        results[request_id] = result

        if args.verbose:
            label = "ok" if result.ok else "FAIL"
            detail = result.status if result.status is not None else result.error
            print(f"[w{worker_id:02d}] #{request_id:04d} {label} {detail} {result.latency_ms:.1f}ms")

        queue.task_done()


async def wait_for_server(host: str, port: int, timeout: float) -> None:
    deadline = time.monotonic() + timeout
    last_error = None
    while time.monotonic() < deadline:
        result = await send_one(host, port, "/__startup_probe__", -1, min(0.5, timeout))
        if result.ok:
            return
        last_error = result.error or f"status {result.status}"
        await asyncio.sleep(0.05)
    raise TimeoutError(f"server did not accept connections on {host}:{port}: {last_error}")


def percentile(values: List[float], pct: float) -> float:
    if not values:
        return 0.0
    index = max(0, min(len(values) - 1, round((len(values) - 1) * pct)))
    return sorted(values)[index]


def print_summary(results: List[Result], elapsed_s: float) -> None:
    successes = [r for r in results if r.ok]
    failures = [r for r in results if not r.ok]
    latencies = [r.latency_ms for r in successes]

    print()
    print(f"requests: {len(results)} total, {len(successes)} ok, {len(failures)} failed")
    print(f"elapsed:  {elapsed_s:.3f}s ({len(results) / elapsed_s:.1f} req/s)")
    if latencies:
        print(
            "latency:  "
            f"min {min(latencies):.1f}ms, "
            f"mean {statistics.fmean(latencies):.1f}ms, "
            f"p50 {percentile(latencies, 0.50):.1f}ms, "
            f"p95 {percentile(latencies, 0.95):.1f}ms, "
            f"max {max(latencies):.1f}ms"
        )

    if failures:
        print()
        print("first failures:")
        for result in failures[:10]:
            print(f"  status={result.status} error={result.error} latency={result.latency_ms:.1f}ms")


async def run_load(args: argparse.Namespace) -> int:
    paths = [path if path.startswith("/") else f"/{path}" for path in args.paths.split(",")]
    queue = asyncio.Queue()
    for request_id in range(args.requests):
        queue.put_nowait(request_id)

    results = [Result(False, None, 0, 0, "not run") for _ in range(args.requests)]
    concurrency = min(args.concurrency, args.requests)
    started = time.perf_counter()
    workers = [
        asyncio.create_task(worker(worker_id, queue, args, paths, results))
        for worker_id in range(concurrency)
    ]
    await asyncio.gather(*workers)
    elapsed_s = time.perf_counter() - started

    print_summary(results, elapsed_s)
    return 0 if all(result.ok for result in results) else 1


async def amain(args: argparse.Namespace) -> int:
    server = None
    if args.server_cmd:
        cmd = shlex.split(args.server_cmd)
        print(f"starting server: {' '.join(cmd)}")
        env = os.environ.copy()
        if args.uv_threadpool_size is not None:
            env["UV_THREADPOOL_SIZE"] = str(args.uv_threadpool_size)
            print(f"server env: UV_THREADPOOL_SIZE={args.uv_threadpool_size}")
        server = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env)
        try:
            await wait_for_server(args.host, args.port, args.startup_timeout)
        except Exception:
            stdout, stderr = server.communicate(timeout=1)
            if stdout:
                print(stdout.decode(errors="replace"), file=sys.stdout)
            if stderr:
                print(stderr.decode(errors="replace"), file=sys.stderr)
            raise

    try:
        return await run_load(args)
    finally:
        if server is not None:
            server.terminate()
            try:
                server.wait(timeout=2)
            except subprocess.TimeoutExpired:
                server.kill()
                server.wait(timeout=2)


def parse_args(argv: Sequence[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Send concurrent HTTP requests to dogfood/http/main.k1."
    )
    parser.add_argument("-n", "--requests", type=int, default=100, help="total requests to send")
    parser.add_argument("-c", "--concurrency", type=int, default=20, help="requests in flight")
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=3000)
    parser.add_argument("--paths", default="/", help="comma-separated paths to cycle through")
    parser.add_argument("--timeout", type=float, default=2.0, help="per-request timeout in seconds")
    parser.add_argument(
        "--server-cmd",
        help="optional command to start before testing, e.g. dogfood/http/.k1-out/http",
    )
    parser.add_argument("--startup-timeout", type=float, default=3.0)
    parser.add_argument(
        "--uv-threadpool-size",
        type=int,
        help="set UV_THREADPOOL_SIZE for --server-cmd",
    )
    parser.add_argument("-v", "--verbose", action="store_true")
    args = parser.parse_args(argv)

    if args.requests < 1:
        parser.error("--requests must be at least 1")
    if args.concurrency < 1:
        parser.error("--concurrency must be at least 1")
    if args.uv_threadpool_size is not None and args.uv_threadpool_size < 1:
        parser.error("--uv-threadpool-size must be at least 1")
    return args


if __name__ == "__main__":
    try:
        raise SystemExit(asyncio.run(amain(parse_args(sys.argv[1:]))))
    except KeyboardInterrupt:
        raise SystemExit(130)
