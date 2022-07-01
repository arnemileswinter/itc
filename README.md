# Interval Tree Clocks

A haskell Implementation of the interval tree clock as described in the paper Interval Tree Clocks: A Logical Clock for Dynamic Systems.

Interval Tree Clocks serve as an alternative to vector clocks, in that clock-carriers may join or enter the system after clock initialization.
Therefore it is not required to know beforehand how many actors are carrying clocks in a distributed system.

This is the first package i'm publishing, please don't hesitate to report issues you encounter.
Also I did not yet come to use this in production myself.

The paper this library is based on is Almeida, Paulo & Baquero, Carlos & Fonte, Victor. (2008). Interval Tree Clocks: A Logical Clock for Dynamic Systems. 5401. 259-274. 10.1007/978-3-540-92221-6_18.
