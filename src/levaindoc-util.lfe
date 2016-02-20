(defmodule levaindoc-util
  (doc "Utility functions for [[levaindoc]].")
  (export (random-name 0)))

(include-lib "clj/include/compose.lfe")

(defun random-name ()
  "Generate a random filename, ending in `.temp`."
  (++ (random-string) "-" (timestamp) ".temp"))

(defun random-string ()
  "Generate a random, 11-character, alphanumeric string."
  (random:seed (erlang:monotonic_time)
               (erlang:time_offset)
               (erlang:unique_integer))
  (-> #0x100000000000000
      (random:uniform)
      (integer_to_list 36)
      (string:to_lower)))

(defun timestamp ()
  "Return the current timestamp, as a string."
  (let ((`#(,megasec ,sec ,_microsec) (os:timestamp)))
    (-> (* megasec 1000000) (+ sec) (integer_to_list))))
