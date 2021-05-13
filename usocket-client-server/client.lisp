(defpackage "TEST.CLIENT"
  (:use "COMMON-LISP" "TEST.COMMON")
  (:export "MAIN"))
(in-package "TEST.CLIENT")

(defun telnet-function (socket)
  (let ((buffer   (make-buffer))
        (response (make-buffer :contents '(#|ASCII ACK:|#6))))
    (read-message socket buffer)
    (format t "Client Received: ~S~%" buffer)
    (force-output)
    (write-message socket response)
    (format t "Client Sent:     ~S~%" response)
    (force-output)
    ;; To wait the disconnection:
    (read-message socket response)))

(defun start-telnet-client (&optional (telnet-function 'telnet-function))
  (usocket:with-connected-socket
      (socket-handle
       (usocket:socket-connect "127.0.0.1" 3270
                               :protocol :stream
                               :element-type '(unsigned-byte 8)))
    (funcall telnet-function (usocket:socket-stream socket-handle))))


(defun main (&rest arguments)
  (declare (ignore arguments))
  (start-telnet-client)
  0)

;;;; THE END ;;;;

