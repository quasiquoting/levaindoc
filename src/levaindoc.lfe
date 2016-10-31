;;; ========================================================== [ levaindoc.lfe ]

(defmodule levaindoc
  "A partial LFE port of [Pandex](https://github.com/FilterKaapi/pandex)."
  ;; API
  (export (convert-string 1) (convert-string 3) (convert-string 4)
          (convert-file   1) (convert-file   3) (convert-file   4)))

(include-lib "lfe/include/clj.lfe")

;;; ============================================================ [ Conversions ]

(eval-when-compile
  (defun -name
    (['file input output]
     (list_to_atom (++ input "-file->" output)))
    (['string input output]
     (list_to_atom (++ input "->" output))))
  (defun -doc
    (['file 1 input output]
     (++ "Read the file `file` and convert its "
         input "-formatted contents to " output "."))
    (['file 2 input output]
     (++ "Read the file `file` and convert its "
         input "-formatted contents to " output ".\n  "
         "N.B. `_options` are currently ignored."))
    (['string 1 input output]
     (++ "Given a `string` in " input " format, convert it to " output "."))
    (['string 2 input output]
     (++ "Given a `string` in " input " format, convert it to " output ".\n  "
         "N.B. `_options` are currently ignored.")))
  (defun -fun (source)
    (list_to_atom (++ "convert-" (atom_to_list source))))
  (defun defconv (source input output)
    (let ((|-CONV-| (-name source input output)))
      `(progn
         (defn , |-CONV-| (,source  _options)
           ,(-doc source 2 input output)
           (,(-fun source) ,source ,input ,output))
         (defn ,|-CONV-| (,source)
           ,(-doc source 1 input output)
           (,|-CONV-| ,source []))))))

(defmacro defconversions ()
  "For each supported `input`/`output` pair, define the conversion functions
  `{{input-}}->{{output}}/{1,2}` and `{{input}}-file->{{output}}/{1,2}`,
  for strings and files, respectively."
  `(progn
     ,@(lc ((<- input   (levaindoc-util:input-formats))
            (<- output  (levaindoc-util:output-formats))
            (<- source '[file string]))
         (defconv source input output))))

(defconversions)

;;; ==================================================================== [ API ]

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
  (let* ((dot-temp ".temp")
         ('ok (filelib:ensure_dir (filename:join dot-temp "dummy")))
         (name (doto (filename:join dot-temp (levaindoc-util:random-name))
                 (file:write_file string)))
         (`#(ok ,output) (convert-file name from to options)))
    (file:delete name)
    `#(ok ,output)))

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

;;; ==================================================================== [ EOF ]
