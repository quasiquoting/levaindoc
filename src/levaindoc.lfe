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
;; with names of the form, {{input-format}}->{{output-format}}.
;; See include/conversions.lfe for details.
(defconversions)


;;;===================================================================
;;; API
;;;===================================================================

(defun convert-string (string)
  "Equivalent to `markdown_github->html/1`, which calls [[convert-string/3]].

See [conversions](conversions.html) and [levaindoc-util](levaindoc-util.html)."
  (markdown_github->html string))

(defun convert-string (string from to)
  "Equivalent to `(`[[convert-string/4]] `string from to [])`."
  (convert-string string from to []))

(defun convert-string (string from to options)
  "Convert a `string` from input format `from` to output format `to`.

Write `string` to a random temporary file ([[levaindoc-util:random-name/0]]);
call [[convert-file/4]], capturing the output; delete the temporary file
and return `` `#(ok ,output) ``."
  (let ((dot-temp ".temp"))
    (when-not (filelib:is_dir dot-temp) (file:make_dir dot-temp))
    (let ((name (filename:join dot-temp (levaindoc-util:random-name))))
      (file:write_file name string)
      (let ((`#(ok ,output) (convert-file name from to options)))
        (file:delete name)
        `#(ok ,output)))))

(defun convert-file (file)
  "Equivalent to `markdown_github-file->html/1`, which calls [[convert-file/3]].

See [conversions](conversions.html) and [levaindoc-util](levaindoc-util.html)."
  (markdown_github-file->html file))

(defun convert-file (file from to)
  "Equivalent to `(`[[convert-file/4]] `file from to [])`."
  (convert-file file from to []))

(defun convert-file (file from to _options)
  "Convert a `file` from input format `from` to output format `to`.
Return `` `#(ok ,output) ``.

A list of `_options` is currently ignored."
  (let ((output (os:cmd (++ "pandoc " file " -f " from " -t " to))))
    `#(ok ,output)))