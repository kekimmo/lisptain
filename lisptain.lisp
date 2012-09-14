(require 'sb-bsd-sockets)

(defun confirm-protocol (h)
 (string= (read-line h) "LISTAIN 001"))

(defun create-socket ()
 (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))

(defun string-empty-p (str)
  (string= str ""))


(defun cut (str)
  (let ((first-space (position #\Space str)))
	(if (not first-space)
	  (values str "")
	  (values (subseq str 0 first-space) (subseq str (1+ first-space))))))
 

(defun words (str)
  (multiple-value-bind (first rest) (cut str)
	(if (string-empty-p first)
	  (if (string-empty-p rest)
		(list)
		(words rest))
	  (cons first (words rest)))))

(defun read-msg (h)
 (let* ((line (read-line h))
	   (parts (words line)))
  (if parts
   (values-list parts)
   (read-msg h))))

(defun parse-id-line (str)
  (multiple-value-bind (id_str data) (cut str)
	(list (parse-integer id_str) data)))

(defun read-len (h)
  (multiple-value-bind (foo len-str) (read-msg h)
	(parse-integer len-str)))

(defun read-id-lines (h)
  (loop repeat (read-len h)
		collect (parse-id-line (read-line h))))

(defun read-lists (h)
  (write-line "LISTS" h)
  (read-id-lines h))

(defun read-list (h list-id)
  (format t "Reading list ~a...~%" list-id)
  (write-line (format nil "LIST ~a" list-id) h)
  (read-id-lines h))

(defun print-list (l)
  (loop for (item-id text) in l
		do (format t "~D ~a~%" item-id text)))

(defun main ()
 (let ((s (create-socket)))
  (unwind-protect
   (progn
	(sb-bsd-sockets:socket-connect s #(82 130 32 73) 11511)
	(let ((h (sb-bsd-sockets:socket-make-stream s :input t :output t :buffering :line)))
	 (confirm-protocol h)
	 (loop for (list-id name) in (read-lists h)
		   do (print-list (read-list h list-id)))
	)
   )
   (sb-bsd-sockets:socket-close s)
  )
 )
)


(main)

