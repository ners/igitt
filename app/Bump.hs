module Bump where

import Params
import Push
import WorkingBranch
import Prelude

{-
 - igitt bump ->
 -     "I have done some new work. I want to push it to N and start the next piece of work on N+1."
 -     if you are currently on <name>-<N>, it
 -          <igitt push>
 -          creates new <name>-<N+1>
 -     otherwise, fails, you should use igitt new
 -}
bump :: Params Identity -> PushParams Maybe -> IO WorkingBranch
bump params pushParams = do
    currBranch <- push params pushParams
    let nextBranch = succBranch currBranch
    run_ ["git", "checkout", "-b", showBranch nextBranch]
    pure nextBranch
