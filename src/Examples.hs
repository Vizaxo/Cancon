module Examples where

import Interpreter
import Expr

badParse :: Either String Stack
badParse = interpret "["
-- => Parse error

badIdentifier :: Either String Stack
badIdentifier = interpret "invalid"
-- => Undefined identifier invalid

badType :: Either String Stack
badType = interpret "drop"
-- => Type checking failed.

badApplyApply :: Either String Stack
badApplyApply = interpret "[apply] apply"
-- => Type checking failed.

runApplyId :: Either String Stack
runApplyId = interpret "[id] apply"
-- => Stack: Empty

runId :: Either String Stack
runId = interpret "id"
-- => Stack: Empty

runQuote :: Either String Stack
runQuote = interpret "[dup]"
-- => Stack: dup

runDup :: Either String Stack
runDup = interpret "[dup] dup"
-- => Stack: dup dup

runApply :: Either String Stack
runApply = interpret "[drop] [dup] apply"
-- => Stack: drop drop

runCompose :: Either String Stack
runCompose = interpret "[drop] [apply] [swap] [drop] compose apply"
-- => Stack: apply
