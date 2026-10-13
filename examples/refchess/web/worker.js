import { wasiImports } from '../../wasi.js'

const POST_INTERVAL_MS = 30

let wasm = null
let lastPost = -Infinity

const wasi = wasiImports(() => wasm.memory, (text) => console.log(text))

const host = {
  progress(nodes, boardPtr, from, to) {
    const now = performance.now()
    if (now - lastPost < POST_INTERVAL_MS) return
    lastPost = now
    const board = new Uint8Array(wasm.memory.buffer, boardPtr, 64).slice()
    postMessage({ type: 'progress', nodes: Number(nodes), board, from, to })
  },
}

const bytes = await (await fetch('../refchess.wasm')).arrayBuffer()
if (new DataView(bytes).getUint32(0, true) !== 0x6d736100) {
  postMessage({ type: 'error', message: 'the demo is not built for the browser: run `just site`' })
  throw new Error('not a wasm module')
}
const { instance } = await WebAssembly.instantiate(bytes, { wasi_snapshot_preview1: wasi, host })
wasm = { exports: instance.exports, memory: instance.exports.memory }

onmessage = (e) => {
  const t0 = performance.now()
  const nodes = Number(wasm.exports.k1_perft(e.data.depth))
  postMessage({ type: 'done', nodes, ms: performance.now() - t0 })
}

postMessage({ type: 'ready' })
