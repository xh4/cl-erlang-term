(in-package :erlang-term)

;;;;
;;;; Erlang map
;;;;

(defconstant +map-ext+ 116)

(defclass erlang-map (erlang-object)
  ((pairs :reader pairs :initarg :pairs))
  (:documentation "Erlang map."))

;;;
;;; Methods
;;;

(defmethod print-object ((object erlang-map) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "{~{~s~^ ~}}" (pairs object))))

(defun make-map (alist)
  "Create an Erlang map"
  (make-instance 'erlang-map
                 :pairs alist))

(defun map-ref (map pos)
  (nth pos (pairs map)))

(defun map-arity (map)
  (length (pairs map)))

(defmethod arity ((x erlang-map))
  "The number of pairs of Erlang map X."
  (map-arity x))

(defmethod size ((x erlang-map))
  "The number of pairs of Erlang map X."
  (map-arity x))

;;;
;;; Encode/Decode
;;;

;; MAP_EXT
;; +-----+-------+-------+
;; |  1  |   4   |   N   |
;; +-----+-------+-------+
;; | 116 | Arity | Pairs |
;; +-----+-------+-------+
;;

(defmethod encode-erlang-object ((map erlang-map))
  (concatenate 'nibbles:simple-octet-vector
               (vector +map-ext+)
               (uint32-to-bytes (map-arity map))
               (mapconc-pairs #'encode-erlang-object (pairs map))))

(defun mapconc-pairs (fn pairs)
  (loop
     for pair in pairs
     collect (funcall fn (car pair)) into mapped-pairs
     collect (funcall fn (cdr pair)) into mapped-pairs
     finally (return (apply #'concatenate
                            `(nibbles:simple-octet-vector ,@mapped-pairs)))))

(defmethod decode-erlang-object ((tag (eql +map-ext+)) bytes pos)
  (let ((arity (bytes-to-uint32 bytes pos)))
    (multiple-value-bind (pairs new-pos)
        (decode-map-pairs bytes arity (+ 4 pos))
      (values (make-instance 'erlang-map :pairs pairs)
              new-pos))))

(defun decode-map-pairs (bytes arity pos)
  (loop
     repeat arity
     for (key value) = (let ((key (multiple-value-bind (key new-pos)
                                      (decode bytes :start pos :version-tag nil)
                                    (setf pos new-pos)
                                    key))
                             (value (multiple-value-bind (value new-pos)
                                        (decode bytes :start pos :version-tag nil)
                                      (setf pos new-pos)
                                      value)))
                         (list key value))
     collect (cons key value) into pairs
     finally (return (values pairs pos))))
