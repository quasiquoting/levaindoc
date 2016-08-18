(defmodule levaindoc
  "A partial LFE port of [Pandex](https://github.com/FilterKaapi/pandex)."
  ;; API
  (export (convert-string 1) (convert-string 3) (convert-string 4)
          (convert-file   1) (convert-file   3) (convert-file   4)))

;;;===================================================================
;;; Conversions
;;;===================================================================

(eval-when-compile
  (defun defn (name args doc body)
    "Similar to Clojure's `defn`. Define and export a function."
    `(progn (defun ,name ,args ,doc ,body)
            (extend-module () ((export (,name ,(length args)))))))
  (defun defconv (input output)
    "Define conversion functions from `input` to `output`."
    `(progn
       ,@(let ((|-CONV-| (list_to_atom (++ input "->" output))))
           `(,(defn |-CONV-| '(string _options)
                (++ "Given a `string` in " input " format, "
                    "convert it to " output ".\n  "
                    "N.B. `_options` are currently ignored.")
                `(convert-string string ,input ,output))
             ,(defn |-CONV-| '(string)
                (++ "Given a `string` in " input " format, "
                    "convert it to " output ".")
                `(,|-CONV-| string []))))
       ,@(let ((|-FILE-CONV-| (list_to_atom (++ input "-file->" output))))
           `(,(defn |-FILE-CONV-| '(file _options)
                (++ "Read the file `file` and convert its "
                    input "-formatted contents to " output ".\n  "
                    "N.B. `_options` are currently ignored.")
                `(convert-file file ,input ,output))
             ,(defn |-FILE-CONV-| '(file)
                (++ "Read the file `file` and convert its "
                    input "-formatted contents to " output ".")
                `(,|-FILE-CONV-| file [])))))))

(defmacro defconversions ()
  "Call [[defconv/2]] on every supported `input`/`output` pair."
  `(progn ,@(lc ((<- input  (levaindoc-util:input-formats))
                 (<- output (levaindoc-util:output-formats)))
              (defconv input output))))

;; Define all the conversion functions, unary and binary, for strings and files,
;; with the names, {{input-}}->{{output}} and {{input}}-file->{{output}}.
(defconversions)


;;;===================================================================
;;; API
;;;===================================================================

(defun convert-string (string)
  "Equivalent to `markdown_github->html/1`, which calls [[convert-string/3]].

  See [conversions][] and [levaindoc-util][].

  [conversions]: conversions.html
  [levaindoc-util]: levaindoc-util.html"
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
    (if (filelib:is_dir dot-temp) 'ok (file:make_dir dot-temp))
    (let ((name (filename:join dot-temp (levaindoc-util:random-name))))
      (file:write_file name string)
      (let ((`#(ok ,output) (convert-file name from to options)))
        (file:delete name)
        `#(ok ,output)))))

(defun convert-file (file)
  "Equivalent to `markdown_github-file->html/1`, which calls [[convert-file/3]].

  See [conversions][] and [levaindoc-util][].

  [conversions]: conversions.html
  [levaindoc-util]: levaindoc-util.html"
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
