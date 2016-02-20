(defmodule levaindoc
  (doc "A partial LFE port of [Pandex](https://github.com/FilterKaapi/pandex).")
  (import (rename erlang ((list_to_atom 1) list->atom)))
  ;; API
  (export (convert-string 1) (convert-string 3) (convert-string 4)
          (convert-file   1) (convert-file   3) (convert-file   4))
  ;; GFM->HTML
  (export (markdown_github->html 1) (markdown_github->html 2))
  #| (export-macro defconv) |#)

(include-lib "clj/include/compose.lfe")

;; TODO: Push local clj changes and make a few PRs
;; (include-lib "clj/include/conditionals.lfe")
(defmacro when-not
  "If `test` evaluates to `false`, evaluate `body` in an implicit `progn`,
otherwise if `test` evaluates to `true`, return `undefined`."
  (`(,test . ,body)
   `(if ,test 'undefined (progn ,@body))))

(defmacro defconv (reader writer)
  "Define a conversion from `reader` to `writer`."
  `(progn
     (defun ,(list_to_atom (++ reader "->" writer)) (string _options)
       (convert-string string ,reader ,writer))
     (defun ,(list_to_atom (++ reader "->" writer)) (string)
       (,(list_to_atom (++ reader "->" writer)) string []))))

(defconv "markdown_github" "html")


;; TODO: Get this to work!
#|
(progn
  (eval-when-compile
    (lc ((<- reader '["markdown" "markdown_github" "markdown_strict"
                      "markdown_mmd" "markdown_phpextra" "commonmark"
                      "json" "rst" "textile" "html" "latex"])
         (<- writer '["json" "html" "html5" "s5" "slidy" "dzslides"
                      "docbook" "man" "opendocument" "latex" "beamer"
                      "context" "texinfo" "markdown" "markdown_github"
                      "markdown_strict" "markdown_mmd" "markdown_phpextra"
                      "commonmark" "plain" "rst" "mediawiki" "textile"
                      "rtf" "org" "asciidoc"]))
      `(defun ,(list_to_atom (++ reader "->" writer)) (string)
         (convert-string string ,reader ,writer)))))
|#


;;;===================================================================
;;; API
;;;===================================================================

(defun convert-string (string)
  "Equivalent to [[markdown_github->html/1]]."
  (markdown_github->html string))

(defun convert-string (string from to)
  "Equivalent to `(`[[convert-string/4]]` string from to [])`."
  (convert-string string from to []))

(defun convert-string (string from to _options)
  (let ((dot-temp ".temp"))
    (when-not (filelib:is_dir dot-temp) (file:make_dir dot-temp))
    (let ((name (filename:join dot-temp (levaindoc-util:random-name))))
      (file:write_file name string)
      (let ((`#(ok ,output) (convert-file name from to)))
        (file:delete name)
        `#(ok ,output)))))

(defun convert-file (file)
  "Equivalent to `(`[[convert-file/3]]` file \"markdown_github\" \"html\")`."
  (convert-file file "markdown" "html"))

(defun convert-file (file from to)
  "Equivalent to `(`[[convert-file/4]]` file from to [])`."
  (convert-file file from to []))

(defun convert-file (file from to _options)
  "[[convert-file/4]] works under the hood of all the other functions."
  (let ((output (os:cmd (++ "pandoc " file " -f " from " -t " to))))
    `#(ok ,output)))
