;;;; maze.asd

(asdf:defsystem #:maze
  :description "A maze generator."
  :author "Anthony Fairchild <fairchild.anthony@gmail.com>"
  :license "Public Domain"
  :serial t
  :components ((:file "package")
               (:file "maze")))

