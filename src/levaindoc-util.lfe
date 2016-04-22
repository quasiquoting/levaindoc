(defmodule levaindoc-util
  (doc "Utility functions for [levaindoc](levaindoc.html).")
  ;; Conversions
  (export (input-formats 0) (output-formats 0))
  ;; Random filename
  (export (random-name 0))
  (import (from erlang (monotonic_time 0) (time_offset 0) (unique_integer 0))))


;;;===================================================================
;;; Conversions
;;;===================================================================

(defun input-formats ()
  "The list of supported input formats.

```bash
$ pandoc --version | head -1
```
```
pandoc 1.15.0.6
```

```bash
$ pandoc --help | head -6 | tail +2
```
```
Input formats:  commonmark, docbook, docx, epub, haddock, html, json*, latex,
                markdown, markdown_github, markdown_mmd, markdown_phpextra,
                markdown_strict, mediawiki, native, opml, org, rst, t2t,
                textile, twiki
                [ *only Pandoc's JSON version of native AST]
```"
  '["commonmark" "docbook" "docx" "epub" "haddock" "html" "json" "latex"
    "markdown" "markdown_github" "markdown_mmd" "markdown_phpextra"
    "markdown_strict" "mediawiki" "native" "opml" "org" "rst" "t2t"
    "textile" "twki"])

(defun output-formats ()
  "The list of supported output formats.

```bash
$ pandoc --version | head -1
```
```
pandoc 1.15.0.6
```

```bash
$ pandoc --help | tail +7 | head -7
```
```
Output formats: asciidoc, beamer, commonmark, context, docbook, docx, dokuwiki,
                dzslides, epub, epub3, fb2, haddock, html, html5, icml, json*,
                latex, man, markdown, markdown_github, markdown_mmd,
                markdown_phpextra, markdown_strict, mediawiki, native, odt,
                opendocument, opml, org, pdf**, plain, revealjs, rst, rtf, s5,
                slideous, slidy, texinfo, textile
                [**for pdf output, use latex or beamer and -o FILENAME.pdf]
```"
  '["asciidoc" "beamer" "commonmark" "context" "docbook" "docx" "dokuwiki"
    "dzslides" "epub" "epub3" "fb2" "haddock" "html" "html5" "icml" "json"
    "latex" "man" "markdown" "markdown_github" "markdown_mmd"
    "markdown_phpextra" "markdown_strict" "mediawiki" "native" "odt"
    "opendocument" "opml" "org" "pdf" "plain" "revealjs" "rst" "rtf" "s5"
    "slideous" "slidy" "texinfo" "textile"])


;;;===================================================================
;;; Random filename
;;;===================================================================

(defun random-name ()
  "Generate a random filename, ending in `.temp`."
  (++ (random-string) "-" (timestamp) ".temp"))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun random-string ()
  "Generate a random, 11-character, alphanumeric string."
  (random:seed (monotonic_time) (time_offset) (unique_integer))
  (string:to_lower (integer_to_list (random:uniform #0x100000000000000) 36)))

(defun timestamp ()
  "Return the current timestamp, as a string."
  (let ((`#(,megasec ,sec ,_microsec) (os:timestamp)))
    (integer_to_list (+ sec (* megasec 1000000)))))
