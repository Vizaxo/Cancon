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
