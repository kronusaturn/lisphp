(in-package :asdf)

(defsystem :lisphp
    :depends-on ("hunchentoot")
    :components ((:file "lisphp")))
