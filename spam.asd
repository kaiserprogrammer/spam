(asdf:defsystem spam
  :name "spam"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Jürgen Bickert <kaiserprogrammer@gmail.com>"
  :licence "BSD"
  :description "Spam filter"
  :long-description ""
  :components
  ((:file "spam"))
  :depends-on (:cl-ppcre))


