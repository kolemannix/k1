---
title: 417 million strings per second
weight: 5
charts: [int-strings]
suites: [int-strings]
---

Daniel Lemire's strings-per-second loop, `buf[i & 1023] = to_string(i)` for a hundred million integers. The faded rows are Lemire's own numbers on his machine; the C++ row and the k1 row are this machine, where k1's `spill-string` is a plain library type with no compiler support. How `show()` got here, and what `spill-string` is, is on the blog: [417 million strings per second in k1](/blog/417-million-strings-per-second-in-k1/).
