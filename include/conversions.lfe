(defmacro defconv (input output)
  "Define a conversion from `input` to `output`."
  `(progn
     (defun ,(list_to_atom (++ input "->" output)) (string _options)
       (convert-string string ,input ,output))
     (defun ,(list_to_atom (++ input "->" output)) (string)
       (,(list_to_atom (++ input "->" output)) string []))))

(defmacro defconversions ()
  "Call [[defconv/2]] on every supported `input`/`output` pair."
  `(progn . ,(lc ((<- input  (levaindoc-util:input-formats))
                  (<- output (levaindoc-util:output-formats)))
               `(defconv ,input ,output))))
