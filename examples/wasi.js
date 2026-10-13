export function wasiImports(memory, onStdout) {
  const decoder = new TextDecoder()
  const view = () => new DataView(memory().buffer)
  return {
    proc_exit(code) { throw { k1exit: code } },
    fd_write(fd, iovs, iovsLen, writtenPtr) {
      const v = view()
      let written = 0
      for (let i = 0; i < iovsLen; i++) {
        const buf = v.getUint32(iovs + i * 8, true)
        const len = v.getUint32(iovs + i * 8 + 4, true)
        onStdout(decoder.decode(new Uint8Array(memory().buffer, buf, len)))
        written += len
      }
      v.setUint32(writtenPtr, written, true)
      return 0
    },
    fd_read(fd, iovs, iovsLen, readPtr) { view().setUint32(readPtr, 0, true); return 0 },
    fd_fdstat_get(fd, statPtr) { new Uint8Array(memory().buffer)[statPtr] = 0; return 0 },
    clock_time_get(id, precision, timePtr) {
      view().setBigUint64(timePtr, BigInt(Math.round(performance.now() * 1e6)), true)
      return 0
    },
    poll_oneoff(subs, events, count, countPtr) { view().setUint32(countPtr, 0, true); return 0 },
    environ_sizes_get(countPtr, sizePtr) {
      const v = view()
      v.setUint32(countPtr, 0, true)
      v.setUint32(sizePtr, 0, true)
      return 0
    },
    environ_get: () => 0,
  }
}
