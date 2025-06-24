module Bump where

import Params
import Push
import WorkingBranch
import Prelude

{-
 - igitt bump ->
 -     "I have done some new work. I want to push it to N+1 and leave N unchanged."
 -     if you are currently on <name>-<N>, it
 -          creates new <name>-<N+1>
 -          <igitt push>
 -     otherwise, fails, you should use igitt new
 -}
bump :: Params Identity -> PushParams Maybe -> IO WorkingBranch
bump params pushParams = do
    switchToNextWorkingBranch =<< getCurrWorkingBranch
    push params pushParams
