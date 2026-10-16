# Helme wishlist: open items (2026-10-12)

From the "Candidates for k1 std/core" section of `helme2/PLAN.md`. Numbers are the original
bullets. Done and dropped: 2, 4, 5, 6, 7, 10b, 11, 12, 13, 15, 16, 19, 21, 22; delete them from PLAN.md.
Sizes: S up to a day, M a few days, L a week or more.

- **3. pubsub topic index (M).** `publish` clones the topic, `dispatch` is O(subscribers) per
  event. Only httpapp and http_test use it. Plan: interned topic ids, subscribers indexed by id,
  multi-topic subscribe. Decide: improve it, or leave it as demo plumbing until a user appears.
