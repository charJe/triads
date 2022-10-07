(in-package asdf-user)

(defsystem charje.triads
  :description "Music composition tool to convert roman numerals into triads"
  :long-description "Triads is a simple command line tool that reads roman
numeral notation from standard input (or a file) and an musical key and outputs
the roman numeral in addition to the notes of the triad associated with that
roman numeral given the key."
  :version "0"
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "GPLv3"
  :depends-on
  ("serapeum"
   "str"
   "trivia")
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "generate-scale")
     (:file "key")
     (:file "roman-to-triad")
     (:file "command-line"))))
  :build-operation "program-op"
  :entry-point "charje.triads:main"
  :build-pathname #.(merge-pathnames #p"triads" (uiop:getcwd)))
