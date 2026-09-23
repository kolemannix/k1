(() => {
  const treemap = document.getElementById("treemap");
  const crumbs = document.getElementById("size-crumbs");
  const stats = document.getElementById("size-stats");
  const tip = document.getElementById("size-tip");

  const hueOf = (node, parentHue, index, depth) => {
    if (depth === 0) return 210;
    if (depth === 1) return (index * 137.508) % 360;
    return parentHue;
  };
  const prepare = (node, parent, hue, depth) => {
    node.parent = parent;
    node.hue = hue;
    if (node.c) {
      node.c.forEach((child, i) => prepare(child, node, hueOf(child, hue, i, depth + 1), depth + 1));
      node.c.sort((a, b) => b.total - a.total);
      node.total = node.c.reduce((sum, child) => sum + child.total, 0);
      node.leaves = node.c.reduce((sum, child) => sum + child.leaves, 0);
    } else {
      node.total = node.v;
      node.leaves = 1;
    }
  };
  prepare(SIZE_TREE, null, 210, 0);

  const pathOf = (node) => {
    const names = [];
    for (let n = node; n; n = n.parent) names.unshift(n.n);
    return names;
  };
  const fmt = (n) => n.toLocaleString();

  // Squarified treemap (Bruls, Huizing, van Wijk): rows along the shorter
  // side, each row closed when adding the next item worsens its aspect ratio
  const squarify = (items, x, y, w, h) => {
    const rects = [];
    const total = items.reduce((sum, item) => sum + item.total, 0);
    if (total <= 0 || w <= 0 || h <= 0) return rects;
    const scale = (w * h) / total;
    let i = 0;
    while (i < items.length) {
      const horizontal = w >= h;
      const side = horizontal ? h : w;
      const row = [];
      let rowArea = 0;
      let best = Infinity;
      while (i < items.length) {
        const area = items[i].total * scale;
        if (area <= 0) { i++; continue; }
        const candidateArea = rowArea + area;
        const thickness = candidateArea / side;
        let worst = 0;
        for (const item of row.concat(items[i])) {
          const len = (item.total * scale) / thickness;
          worst = Math.max(worst, thickness / len, len / thickness);
        }
        if (worst > best && row.length > 0) break;
        row.push(items[i]);
        rowArea = candidateArea;
        best = worst;
        i++;
      }
      if (row.length === 0) break;
      const thickness = rowArea / side;
      let offset = 0;
      for (const item of row) {
        const len = (item.total * scale) / thickness;
        rects.push(horizontal
          ? { item, x: x, y: y + offset, w: thickness, h: len }
          : { item, x: x + offset, y: y, w: len, h: thickness });
        offset += len;
      }
      if (horizontal) { x += thickness; w -= thickness; } else { y += thickness; h -= thickness; }
    }
    return rects;
  };

  const HEAD = 14;
  const PAD = 2;
  const render = (node, el, x, y, w, h) => {
    if (!node.c) return;
    for (const r of squarify(node.c, x, y, w, h)) {
      const child = r.item;
      const tile = document.createElement("div");
      tile.className = "tile k-" + child.k;
      tile.style.left = r.x + "px";
      tile.style.top = r.y + "px";
      tile.style.width = r.w + "px";
      tile.style.height = r.h + "px";
      tile.dataset.node = child.id;
      const hue = child.hue;
      if (child.c) {
        tile.style.borderColor = `hsl(${hue} 40% 50% / 0.6)`;
        tile.style.background = `hsl(${hue} 35% 50% / 0.08)`;
        const showHead = r.w >= 40 && r.h >= HEAD + 8;
        if (showHead) {
          const head = document.createElement("div");
          head.className = "tile-head";
          head.textContent = child.n + (child.k === "ns" ? "" : " ×" + child.leaves);
          tile.appendChild(head);
        }
        const top = showHead ? HEAD : PAD;
        const innerW = r.w - 2 * PAD;
        const innerH = r.h - top - PAD;
        if (innerW >= 6 && innerH >= 6) {
          render(child, tile, PAD, top, innerW, innerH);
        }
      } else {
        const light = child.k === "specialization" ? 72 : child.k === "lambda" ? 36 : 48;
        const sat = child.k === "specialization" ? 60 : 45;
        tile.style.background = `hsl(${hue} ${sat}% ${light}%)`;
        tile.style.color = light > 60 ? "#1d2024" : "#ffffff";
        if (r.w >= 28 && r.h >= 12) tile.textContent = child.n;
      }
      el.appendChild(tile);
    }
  };

  const nodesById = [];
  const assignIds = (node) => {
    node.id = nodesById.length;
    nodesById.push(node);
    if (node.c) node.c.forEach(assignIds);
  };
  assignIds(SIZE_TREE);

  let current = SIZE_TREE;
  const show = (node) => {
    current = node;
    treemap.replaceChildren();
    render(node, treemap, 0, 0, treemap.clientWidth, treemap.clientHeight);
    crumbs.replaceChildren();
    const chain = [];
    for (let n = node; n; n = n.parent) chain.unshift(n);
    chain.forEach((n, i) => {
      if (i > 0) {
        const sep = document.createElement("span");
        sep.className = "crumb-sep";
        sep.textContent = "/";
        crumbs.appendChild(sep);
      }
      const a = document.createElement("a");
      a.textContent = n.n;
      if (n !== node) a.onclick = () => show(n);
      else a.className = "crumb-here";
      crumbs.appendChild(a);
    });
    stats.textContent = `${fmt(node.total)} instructions · ${fmt(node.leaves)} functions`;
  };

  treemap.addEventListener("click", (evt) => {
    const tile = evt.target.closest(".tile");
    if (!tile) return;
    const node = nodesById[tile.dataset.node];
    if (node.c) { evt.stopPropagation(); show(node); }
  });
  treemap.addEventListener("mousemove", (evt) => {
    const tile = evt.target.closest(".tile");
    if (!tile) { tip.hidden = true; return; }
    const node = nodesById[tile.dataset.node];
    const share = current.total ? (100 * node.total / current.total).toFixed(1) : "0";
    const lines = [
      `${node.k} ${node.n}`,
      `${fmt(node.total)} instructions (${share}% of ${current.n})`,
      pathOf(node).join("/"),
    ];
    if (node.c) lines.push(`${fmt(node.leaves)} functions`);
    if (node.l) lines.push(node.l);
    tip.textContent = lines.join("\n");
    tip.hidden = false;
    const right = evt.clientX + 16 + tip.offsetWidth > window.innerWidth;
    tip.style.left = (right ? evt.clientX - 16 - tip.offsetWidth : evt.clientX + 16) + "px";
    tip.style.top = Math.min(evt.clientY + 16, window.innerHeight - tip.offsetHeight - 8) + "px";
  });
  treemap.addEventListener("mouseleave", () => { tip.hidden = true; });
  document.addEventListener("keydown", (evt) => {
    if (evt.key === "Escape" && current.parent) show(current.parent);
  });
  new ResizeObserver(() => show(current)).observe(treemap);
  show(SIZE_TREE);
})();
