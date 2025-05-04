;;; Loads appropriate SDF code.
;;; NOTE: this file shouldn't be `load`ed directly, as it will force the user back to a REPL
;;; `load` it from the repl before using any other code

(load "./sdf/manager/load.scm")
(manage 'new 'term 'generic-procedures 'combinators)
