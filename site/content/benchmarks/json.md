---
title: JSON
weight: 4
charts: [json]
suites: [json]
---

`std/json`, a plain k1 library with no compiler support, parsing and writing the usual three files against simdjson and serde_json. simdjson is the fastest JSON parser there is. k1 sits between it and serde on parsing, and trades places with both on writing.
