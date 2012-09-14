(require 'sb-bsd-sockets)


(defmethod remote-init (h entity-class &rest rest)
  (let ((entity (apply #'make-instance entity-class :comm-h h rest)))
	(remote-refresh entity)
	entity))


(defclass remote-entity ()
  ((exists :initarg :exists
		   :initform nil
		   :accessor exists)
   (comm-h :initarg :comm-h
		   :reader comm-h)
   (request-cmd :allocation :class
				:reader request-cmd)))


(defun remote-refresh (entity)
  (with-accessors ((ex exists) (h comm-h)) entity
	(write-line (request-line entity) h)
	(remote-read entity h)
  	(setf ex t)))


(defgeneric request-line (entity))

(defmethod request-line ((entity remote-entity))
  (request-cmd entity))
  
(defgeneric remote-read (entity h))


(defclass id-entity (remote-entity)
  ((id :initarg :id
	   :reader id)))

(defmethod request-line ((entity id-entity))
  (format nil "~a ~d" (request-cmd entity) (id entity)))


(defclass named-entity (id-entity)
  ((name :initarg :name
		 :reader name)))


(defclass remote-list (remote-entity)
  ((elems :initarg :elems
		  :initform (list)
		  :reader elems)
   (elem-type :allocation :class
			  :reader elem-type)))

(defmethod remote-read ((lst remote-list) h)
  (setf (slot-value lst 'elems)
		(loop repeat (read-len h)
			  collect (parse-elem (elem-type lst) (read-line h)))))

(defgeneric parse-elem (elem-type str))


(defclass list-list (remote-list)
  ((request-cmd :initform "LISTS")
   (elem-type :initform 'list-ref)))

(defmethod parse-elem ((elem-type (eql 'list-ref)) str)
  (parse-id-line str))
							 

(defclass item-list (id-entity remote-list)
  ((request-cmd :initform "LIST")
   (elem-type :initform 'item)))

(defmethod parse-elem ((elem-type (eql 'item)) str)
  (multiple-value-bind (id text) (parse-id-line str)
	(make-instance 'item :id id :text text)))


(defclass item (id-entity)
  ((request-cmd :initform "ITEM")
   (text :initarg :text
		 :reader text)))


(defun confirm-protocol (h)
 (string= (read-line h) "LISTAIN 001"))

(defun create-socket ()
 (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))

(defun connect-socket (s)
  (sb-bsd-sockets:socket-connect s #(82 130 32 73) 11511))

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


(defun main ()
 (let ((s (create-socket)))
  (unwind-protect
   (progn
	 (connect-socket s)
	 (let ((h (sb-bsd-sockets:socket-make-stream s :input t :output t :buffering :line)))
	   (confirm-protocol h)
	   ()

	   ;(loop for (list-id name) in (read-lists h)
		;	 do (print-list (read-list h list-id)))
	)
   )
   (sb-bsd-sockets:socket-close s)
  )
 )
)


;(main)


(defvar *s* (create-socket))
(connect-socket *s*)
(defvar *h* (sb-bsd-sockets:socket-make-stream *s* :input t :output t :buffering :line))
(confirm-protocol *h*)

(defvar *list-list* (remote-init *h* 'list-list))

(defvar *lists* (loop for (id name) in (elems *list-list*)
					   collect (remote-init *h* 'item-list :id id)))




