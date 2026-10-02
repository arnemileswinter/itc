# Interval Tree Clocks

A haskell Implementation of the interval tree clock as described in the paper Interval Tree Clocks: A Logical Clock for Dynamic Systems.

Light on dependencies!

Interval Tree Clocks serve as an alternative to vector clocks, in that clock-carriers may join or enter the system after clock initialization.
Therefore it is not required to know beforehand how many actors are carrying clocks in a distributed system.

This is the first package i'm publishing, please don't hesitate to report issues you encounter.
Also I did not yet come to use this in production myself.

Thoroughly tested, with both quickcheck and visual debugging in conformance to the paper.

The paper this library is based on is Almeida, Paulo & Baquero, Carlos & Fonte, Victor. (2008). Interval Tree Clocks: A Logical Clock for Dynamic Systems. 5401. 259-274. 10.1007/978-3-540-92221-6_18.

## Migrating from 0.2 to 0.3

`StampComparison` has a new constructor, `Equivalent`, for two stamps that have seen exactly the same events
(a stamp and itself, the two halves of a `fork`, a stamp and its `peek`).
In 0.2 these compared as `Concurrent`. `Concurrent` now strictly means that each stamp has seen events the other has not.
If you pattern match on the result of `stampCompare`, add a case for `Equivalent`.

`sameHistory` is new and is `True` exactly when `stampCompare` gives `Equivalent`.

`event` on an anonymous stamp (the result of `peek`) and `join` on stamps with overlapping identities are errors, as before,
but are now documented as preconditions.
