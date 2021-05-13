(defpackage "TEST.SERVER"
  (:use "COMMON-LISP" "TEST.COMMON")
  (:export "MAIN"))
(in-package "TEST.SERVER")

(defun conn-handle (socket)
  (let ((buffer (make-buffer
                 :contents (map 'vector 'char-code
                                (format nil "HELLO~C~C"
                                        (code-char 13)
                                        (code-char 10)))))
        (response (make-buffer :size 8)))
    (write-message socket buffer)
    (format t "Server Sent:     ~S~%" buffer)
    (force-output)
    (read-message socket response)
    (format t "Server Received: ~S~%" response)
    (force-output)))

(defun conn-example1 (&optional (handle 'conn-handle))
  (usocket:with-socket-listener (conn "127.0.0.1" 3270
                                      :element-type '(unsigned-byte 8))
    (usocket:with-connected-socket (c (usocket:socket-accept conn))
      (funcall handle (usocket:socket-stream c)))))

(defun main (&rest arguments)
  (declare (ignore arguments))
  (conn-example1)
  0)

;;;; THE END ;;;;
