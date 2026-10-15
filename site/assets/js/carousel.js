for (const c of document.querySelectorAll('.carousel')) {
  const track = c.querySelector('.track');
  const cards = [...track.children];
  const dots = [...c.querySelectorAll('.dots button')];
  const pos = c.querySelector('.pos');
  const prev = c.querySelector('.prev');
  const next = c.querySelector('.next');
  const clamp = (i) => Math.max(0, Math.min(cards.length - 1, i));
  const current = () => {
    let best = 0;
    for (let i = 1; i < cards.length; i++) {
      if (Math.abs(cards[i].offsetLeft - track.scrollLeft) < Math.abs(cards[best].offsetLeft - track.scrollLeft)) best = i;
    }
    return best;
  };
  let at = 0;
  const go = (i, behavior = 'smooth') => { at = clamp(i); track.scrollTo({ left: cards[at].offsetLeft, behavior }); };
  const show = () => {
    const i = at = current();
    dots.forEach((d, j) => d.toggleAttribute('aria-current', j === i));
    pos.textContent = `${i + 1} / ${cards.length}`;
    prev.disabled = i === 0;
    next.disabled = i === cards.length - 1;
  };
  prev.addEventListener('click', () => go(at - 1));
  next.addEventListener('click', () => go(at + 1));
  dots.forEach((d) => d.addEventListener('click', () => go(+d.dataset.i)));
  c.addEventListener('keydown', (e) => {
    if (e.key === 'ArrowLeft') { go(at - 1); e.preventDefault(); }
    if (e.key === 'ArrowRight') { go(at + 1); e.preventDefault(); }
  });
  let raf = 0;
  track.addEventListener('scroll', () => { cancelAnimationFrame(raf); raf = requestAnimationFrame(show); });
  const jump = () => {
    const i = cards.findIndex((card) => card.id && '#' + card.id === location.hash);
    if (i >= 0) go(i, 'instant');
  };
  window.addEventListener('hashchange', jump);
  window.addEventListener('load', () => { jump(); show(); });
  jump();
  show();
}

for (const fig of document.querySelectorAll('.show figure')) {
  const togs = [...fig.querySelectorAll('.tog')];
  if (!togs.length) continue;
  const pres = [...fig.querySelectorAll('pre')];
  togs.forEach((t, i) => t.addEventListener('click', () => {
    togs.forEach((u, j) => u.setAttribute('aria-pressed', String(j === i)));
    pres.forEach((p, j) => { p.hidden = j !== i; });
  }));
}
