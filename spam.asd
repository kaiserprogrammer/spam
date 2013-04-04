(asdf:defsystem spam
  :name "spam"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Spam filter"
  :long-description ""
  :components
  ((:file "spam"))
  :depends-on (:cl-ppcre))


