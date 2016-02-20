(defmacro defconv (reader writer)
  "Define a conversion from `reader` to `writer`."
  `(progn
     (defun ,(list_to_atom (++ reader "->" writer)) (string _options)
       (convert-string string ,reader ,writer))
     (defun ,(list_to_atom (++ reader "->" writer)) (string)
       (,(list_to_atom (++ reader "->" writer)) string []))))

(defmacro defconversions ()
  "Call [[defconv/2]] on every supported `reader`/`writer` pair."
  `(progn . ,(lc ((<- reader '["markdown" "markdown_github" "markdown_strict"
                               "markdown_mmd" "markdown_phpextra" "commonmark"
                               "json" "rst" "textile" "html" "latex"])
                  (<- writer '["json" "html" "html5" "s5" "slidy" "dzslides"
                               "docbook" "man" "opendocument" "latex" "beamer"
                               "context" "texinfo" "markdown" "markdown_github"
                               "markdown_strict" "markdown_mmd"
                               "markdown_phpextra" "commonmark" "plain" "rst"
                               "mediawiki" "textile" "rtf" "org" "asciidoc"]))
               `(defconv ,reader ,writer))))
