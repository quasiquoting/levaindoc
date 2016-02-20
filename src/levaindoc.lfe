(defmodule levaindoc
  (doc "A partial LFE port of [Pandex](https://github.com/FilterKaapi/pandex).")
  (import (rename erlang ((list_to_atom 1) list->atom)))
  ;; API
  (export (convert-string 1) (convert-string 3) (convert-string 4)
          (convert-file   1) (convert-file   3) (convert-file   4))
  ;; Conversions
  (export all))

(include-lib "include/conditionals.lfe")

(include-lib "include/conversions.lfe")


;;;===================================================================
;;; Conversions
;;;===================================================================

;; Define all the conversion functions, unary and binary,
;; with names of the form, {{reader}}->{{writer}}.
;; See include/conversions.lfe for details.
(defconversions)


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
