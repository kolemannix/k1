const DEPTH = 6
const REFERENCE = 119060324

const GLYPHS = ['', '♙', '♘', '♗', '♖', '♕', '♔', '', '', '♟', '♞', '♝', '♜', '♛', '♚']
const START = new Uint8Array([
  4, 2, 3, 5, 6, 3, 2, 4,
  1, 1, 1, 1, 1, 1, 1, 1,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  9, 9, 9, 9, 9, 9, 9, 9,
  12, 10, 11, 13, 14, 11, 10, 12,
])

const board = document.getElementById('board')
const bar = document.getElementById('bar')
const count = document.getElementById('count')
const verdict = document.getElementById('verdict')
const run = document.getElementById('run')

const squares = []
for (let row = 0; row < 8; row++) {
  for (let col = 0; col < 8; col++) {
    const cell = document.createElement('div')
    if ((row + col) % 2 === 1) cell.className = 'light'
    board.appendChild(cell)
    squares[(7 - row) * 8 + col] = cell
  }
}

function showBoard(bytes, from = -1, to = -1) {
  for (let sq = 0; sq < 64; sq++) {
    squares[sq].textContent = GLYPHS[bytes[sq]]
    squares[sq].classList.toggle('moved', sq === from || sq === to)
  }
}

const format = (n) => n.toLocaleString('en-US')

function showCount(nodes) {
  count.innerHTML = `<span class="n">${format(nodes)}</span> / ${format(REFERENCE)}`
  bar.style.width = `${Math.min(100, (nodes / REFERENCE) * 100)}%`
}

showBoard(START)
showCount(0)

const worker = new Worker(new URL('worker.js', import.meta.url), { type: 'module' })
worker.onmessage = ({ data }) => {
  if (data.type === 'ready') {
    run.textContent = `Run perft ${DEPTH}`
    run.disabled = false
  } else if (data.type === 'progress') {
    showCount(data.nodes)
    showBoard(data.board, data.from, data.to)
  } else if (data.type === 'error') {
    run.textContent = 'Unavailable'
    verdict.innerHTML = `<span class="bad">${data.message}</span>`
  } else if (data.type === 'done') {
    const ok = data.nodes === REFERENCE
    showCount(data.nodes)
    bar.classList.toggle('bad', !ok)
    verdict.innerHTML = `<span class="${ok ? 'ok' : 'bad'}">${ok ? 'pass' : 'fail'}</span>  ${(data.ms / 1000).toFixed(2)} s`
    showBoard(START)
    run.textContent = 'Run again'
    run.disabled = false
  }
}
worker.onerror = (e) => {
  verdict.innerHTML = `<span class="bad">${e.message}</span>`
}

run.addEventListener('click', () => {
  run.disabled = true
  bar.classList.remove('bad')
  verdict.textContent = ''
  showCount(0)
  worker.postMessage({ depth: DEPTH })
})
