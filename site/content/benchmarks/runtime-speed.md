---
title: Runtime speed
weight: 1
charts: [runtime, brotli]
suites: [runtime, brotli]
---

All of the source programs are written 'idiomatically' in the default style. I didn't pull in `memchr` for Rust's byte-scan, because not
everyone knows to do that. You get it for free in `k1`. C doesn't come with a bump arena - it comes with `malloc`/`free` - but you can easily write one.

`k1` allocates in a bump arena by default, and the entire paradigm is constructed around it. We could add a Bumpalo Rust line and I'm sure it would slap.

The point is to show the reasonable out of the box baseline, and simply prove that `k1` does not cap your performance ceiling.
