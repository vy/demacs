;;; Copyright (c) 2008, Volkan YAZICI <volkan.yazici@gmail.com>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:

;;; - Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.

;;; - Redistributions in binary form must reproduce the above copyright notice,
;;;   this list of conditions and the following disclaimer in the documentation
;;;   and/or other materials provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :demacs)


;;; COMMON ROUTINES

(defun extract-slots (definer)
  (mapcar
   (lambda (spec) (car (ensure-list spec)))
   (funcall (ecase (class-name (class-of definer))
              ((class-definer condition-definer) #'slot-specs-of)
              (struct-definer #'slot-descs-of))
            definer)))


;;; CLASS DEFINER ROUTINES

(defclass class-definer (definer)
  ((superclasses :accessor superclasses-of)
   (slot-specs :accessor slot-specs-of)
   (class-options :accessor class-options-of)))

(defmethod available-definer-options ((definer class-definer))
  (list #\e #\a #\s))

(defmethod restricted-definer-options ((definer class-definer))
  nil)

(defmethod initialize-definer ((definer class-definer))
  (destructuring-bind (name superclasses slot-specs &rest class-options)
      (forms-of definer)
    (unless (consp slot-specs)
      (oerror "Expecting a slot-spec list in options ~s of definer ~
               ~s of type ~s."
              (options-of definer) definer))
    (setf (name-of definer) name
          (superclasses-of definer) superclasses
          (slot-specs-of definer) slot-specs
          (class-options-of definer) class-options))
  (validate-definer-options definer)
  definer)

(defun extract-class-accessors (definer)
  (labels ((get-field (place indicator)
             (when place
               (let ((keyword (first place))
                     (value (second place)))
                 (if (eql keyword indicator)
                     value
                     (get-field (cddr place) indicator))))))
    (remove nil (reduce (lambda (accum slot-spec)
                          (let ((options (rest (ensure-list slot-spec))))
                            (union (list (get-field options :accessor)
                                         (get-field options :writer)
                                         (get-field options :reader))
                                   accum)))
                        (slot-specs-of definer)
                        :initial-value nil))))

(defmethod expand-definer ((definer class-definer))
  `(progn
     (defclass ,(name-of definer) ,(superclasses-of definer)
       ,(slot-specs-of definer)
       ,(class-options-of definer))
     ,@(when (has-option-p definer #\e) `((export ',(name-of definer))))
     ,@(when (has-option-p definer #\s) `((export ',(extract-slots definer))))
     ,@(when (has-option-p definer #\a)
         `((export ',(extract-class-accessors definer))))))


;;; CONDITION DEFINER ROUTINES

(defclass condition-definer (class-definer) ())


;;; STRUCT DEFINER ROUTINES

(defclass struct-definer (definer)
  ((documentation :accessor documentation-of)
   (struct-options :accessor struct-options-of)
   (slot-descs :accessor slot-descs-of)))

(defmethod available-definer-options ((definer struct-definer))
  (list #\e #\a #\s))

(defmethod restricted-definer-options ((definer struct-definer))
  nil)

(defmethod initialize-definer ((definer struct-definer))
  (destructuring-bind (name &rest rest) (forms-of definer)
    (destructuring-bind (documentation &rest slot-descs)
        (if (stringp (car rest)) rest (cons nil rest))
      (setf (name-of definer) (first (ensure-list name))
            (struct-options-of definer) (rest (ensure-list name))
            (documentation-of definer) documentation
            (slot-descs-of definer) slot-descs)))
  (validate-definer-options definer)
  definer)

(defun extract-struct-accessors (definer)
  (mapcar (lambda (slot) (intern (format nil "~:@(~s-~s~)" (name-of definer) slot)))
          (extract-slots definer)))

(defmethod expand-definer ((definer struct-definer))
  `(progn
     (defstruct (,(name-of definer) ,@(struct-options-of definer))
       ,@(when (documentation-of definer) `(,(documentation-of definer)))
       ,@(slot-descs-of definer))
     ,@(when (has-option-p definer #\e) `((export ',(name-of definer))))
     ,@(when (has-option-p definer #\s) `((export ',(extract-slots definer))))
     ,@(when (has-option-p definer #\a)
         `((export ',(extract-struct-accessors definer))))))
