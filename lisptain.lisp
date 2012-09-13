(require 'sb-bsd-sockets)

(defun confirm-protocol (h)
 (string= (read-line h) "LISTAIN 001"))

(defun create-socket ()
 (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))

(defun read-msg (h)
 (let ((line (read-line h))
	   (parts (split-sequence #\Space line)))
  (if parts
   (values-list parts)
   (read-msg h))))

(defun read-lists (h)
 (write-line h "LISTS")
 (let ((len-line (read-line h))
	   (
	   )
 )
 (list (list 1 'a) (list 2 'b)))

(defun main ()
 (let ((s (create-socket)))
  (unwind-protect
   (progn
	(sb-bsd-sockets:socket-connect s #(82 130 32 73) 11511)
	(let ((h (sb-bsd-sockets:socket-make-stream s :input :output :buffering :line)))
	 (confirm-protocol h)
	 (format t "~{~{~a~%~}~}" (read-lists h))
	)
   )
   (sb-bsd-sockets:socket-close s)
  )
 )
)


(main)

