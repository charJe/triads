(in-package asdf-user)

(defsystem charje.triads
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
