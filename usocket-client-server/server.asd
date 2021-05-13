(asdf:defsystem "server"
  :description "A test socket server."
  :author "Pascal J. Bourguignon"
  :version "0.0.0"
  :license "AGPL3"
  :depends-on ("usocket")
  :components ((:file "common")
               (:file "server" :depends-on ("common"))))
