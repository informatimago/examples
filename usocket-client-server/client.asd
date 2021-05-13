(asdf:defsystem "client"
  :description "A test socket client."
  :author "Pascal J. Bourguignon"
  :version "0.0.0"
  :license "AGPL3"
  :depends-on ("usocket")
  :components ((:file "common")
               (:file "client" :depends-on ("common"))))
