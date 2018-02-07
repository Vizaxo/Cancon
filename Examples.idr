module Examples

import Interpreter
import Eval
import Expr

runEmpty : Either String Stack
runEmpty = run []
-- => Stack: Empty

runId : Either String Stack
runId = run [id]
-- => Stack: Empty

quoteId : Either String Stack
quoteId = run [Quote id]
-- => Stack: id

dupDupDrop : Either String Stack
dupDupDrop = run [Quote id, dup, dup, drop]
-- => Stack: id, id

badType : Either String Stack
badType = run [drop]
-- => Error: "Type checking failed."

-- This pushes 2 copies of id to the stack, then builds a function to drop 2 values (in a quoted expression), and applies it.
dropDropApply : Either String Stack
dropDropApply = run [Quote id, dup, Quote (Compose drop drop), apply]
-- => Stack: Empty

runQuote : Either String Stack
runQuote = run [Quote id, quote, apply]
-- => Stack: id

runCompose : Either String Stack
runCompose = run [Quote drop, Quote id, Quote id, compose, apply]
-- => Stack: drop

composePushDup : Either String Stack
composePushDup = run [Quote compose, Quote id, quote, Quote dup, compose, apply]
-- => Stack: compose, id, id

runSwap : Either String Stack
runSwap = run [Quote drop, Quote dup, swap]
-- => Stack: dup, drop
