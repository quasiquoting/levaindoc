;; TODO: Push local clj changes and make a few PRs
;; (include-lib "clj/include/conditionals.lfe")
(defmacro when-not
  "If `test` evaluates to `false`, evaluate `body` in an implicit `progn`,
otherwise if `test` evaluates to `true`, return `undefined`."
  (`(,test . ,body)
   `(if ,test 'undefined (progn ,@body))))
