(defpackage "TEST.COMMON"
  (:use "COMMON-LISP")
  (:export "OCTET"
           "MAKE-BUFFER"
           "READ-MESSAGE"
           "WRITE-MESSAGE"))
(in-package "TEST.COMMON")

(deftype octet () `(unsigned-byte 8))

(defun make-buffer (&key (size 1024 sizep) (contents nil contentsp))
  (let ((buffer (make-array (cond
                              (sizep     size)
                              (contentsp (length contents))
                              (t         size))
                            :element-type 'octet
                            :adjustable t
                            :fill-pointer (if contentsp
                                              (length contents)
                                              size)
                            :initial-element 0)))
    (when contentsp
      (replace buffer contents))
    buffer))


;; Note: don't be confused.
;; With :stream sockets, we receive the socket-stream which is a
;; CL:STREAM in the socket parameter.  So we use CL stream operators.
;; If we used :datagram sockets, we could use the USOCKET:SOCKET
;; directly with the socket-send/socket-receive operators.
;; It would probably be better to use generic functions here…

(defun read-message (socket buffer)
  (setf (fill-pointer buffer) (array-dimension buffer 0))

  ;; With stream sockets, we need to have a way to know when the
  ;; message has been received in full.
  ;; Here, we stop reading when we receive the octet 10 as
  ;; last octet of a message, or when the buffer is full.
  ;; It could be a size received in the header of the message,
  ;; or fixed-length messages.
  (loop
    :with read-count := 0
    :for i :below (length buffer)
    :for byte := (read-byte socket)
    :do (setf (aref buffer i) byte)
        (incf read-count)
    :until (= byte 10)
    :finally (setf (fill-pointer buffer) read-count))

  ;; With :datagram sockets, we can just use socket-receive.
  ;; (multiple-value-bind (received received-size)
  ;;     (usocket:socket-receive socket buffer (length buffer))
  ;;   (declare (ignore received))
  ;;   (setf (fill-pointer buffer) received-size))
  
  buffer)

(defun write-message (socket buffer)
  ;; (usocket:socket-send socket buffer (length buffer))
  (write-sequence buffer socket)
  (force-output socket)
  socket)

;;;; THE END ;;;;
