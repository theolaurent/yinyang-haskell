# Fun with continuations

This is haskell version of the yin yang puzzle
(see https://en.wikipedia.org/wiki/Call-with-current-continuation).
The (undelimited) continuations implementation is inspired from
http://okmij.org/ftp/continuations/undelimited.html,
adapted to a monad transformer to be able to get a MonadIO.
