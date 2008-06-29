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


;;; COMMON VARIABLES

(defclass function-definer (definer)
  ((lambda-list :accessor lambda-list-of)
   (body :accessor body-of)))


;;; COMMON ROUTINES

(defun initialize-function-like-definer (definer &optional extra-options-writer)
  (destructuring-bind (name lambda-list &body body) (forms-of definer)
    (setf (name-of definer) name
          (lambda-list-of definer) lambda-list
          (body-of definer) body))
  (validate-definer-options definer extra-options-writer)
  definer)

(defgeneric declare-optimize (definer)
  (:documentation "Produces optimization declaration for the related definer."))

(defgeneric declare-debug (definer)
  (:documentation "Produces debugging declaration for the related definer."))

(defun expand-function-like-definer (definer function &key method-qualifiers)
  (with-body-parts (declarations docstring body) (body-of definer)
    `(progn
       ,@(when-has-option definer #\i `(declaim (inline ,(name-of definer))))
       (,function
        ,(name-of definer)
        ,@method-qualifiers
        ,(lambda-list-of definer)
         ,@(when docstring `(,docstring))
         ,@(when-has-option definer #\o (declare-optimize definer))
         ,@(when-has-option definer #\d (declare-debug definer))
         ,@declarations
         ,@body)
       ,@(when-has-option definer #\e `(export ',(name-of definer))))))


;;; FUNCTION DEFINER ROUTINES

(defmethod available-definer-options ((definer function-definer))
  (list #\i #\o #\d #\e))

(defmethod restricted-definer-options ((definer function-definer))
  (list (list #\o #\d)))

(defmethod declare-optimize ((definer function-definer))
  `(declare (optimize (speed 3) (debug 0) (safety 2))))

(defmethod declare-debug ((definer function-definer))
  `(declare (optimize (speed 0) (debug 3))))

(defmethod initialize-definer ((definer function-definer))
  (initialize-function-like-definer definer))

(defmethod expand-definer ((definer function-definer))
  (expand-function-like-definer definer 'defun))


;;; MACRO DEFINER ROUTINES

(defclass macro-definer (function-definer) ())

(defmethod available-definer-options ((definer macro-definer))
  (list #\o #\d #\e))

(defmethod expand-definer ((definer macro-definer))
  (expand-function-like-definer definer 'defmacro))


;;; COMPILER-MACRO DEFINER ROUTINES

(defclass compiler-macro-definer (macro-definer) ())

(defmethod expand-definer ((definer compiler-macro-definer))
  (expand-function-like-definer definer 'define-compiler-macro))


;;; METHOD DEFINER ROUTINES

(defclass method-definer (function-definer)
  ((qualifiers :accessor qualifiers-of)))

(defmethod available-definer-options ((definer method-definer))
  (list #\o #\d #\e))

(defmethod initialize-definer ((definer method-definer))
  (initialize-function-like-definer
   definer (lambda (definer extra-options)
             (setf (qualifiers-of definer) extra-options))))

(defmethod expand-definer ((definer method-definer))
  (expand-function-like-definer
   definer 'defmethod :method-qualifiers (qualifiers-of definer)))


;;; GENERIC DEFINER ROUTINES

(defclass generic-definer (function-definer) ())

(defmethod available-definer-options ((definer generic-definer))
  (list #\o #\d #\e))

(defmethod expand-definer ((definer generic-definer))
  (labels ((iter (forms &optional extra-forms declarations)
             (if (endp forms)
                 (mapcar #'reverse (list extra-forms declarations))
                 (let ((form (car forms)))
                   (if (and (consp form) (eql (car form) 'declare))
                       (iter (rest forms)
                             extra-forms
                             (cons form declarations))
                       (iter (rest forms)
                             (cons form extra-forms)
                             declarations))))))
    (destructuring-bind (forms declarations) (iter (body-of definer))
      (let ((name (name-of definer)))
        `(progn
           (defgeneric ,name ,(lambda-list-of definer)
             ,@(when-has-option definer #\o (declare-optimize definer))
             ,@(when-has-option definer #\d (declare-debug definer))
             ,@declarations
             ,@forms)
           ,@(when-has-option definer #\e `(export ',name)))))))


;;; TYPE DEFINER ROUTINES

(defclass type-definer (function-definer) ())

(defmethod available-definer-options ((definer type-definer))
  (list #\e))

(defmethod expand-definer ((definer type-definer))
  (expand-function-like-definer definer 'deftype))


;;; PRINT-OBJECT DEFINER ROUTINES

(defclass print-object-definer (function-definer)
  ((print-identity :type boolean :accessor print-identity-of)
   (print-type :type boolean :accessor print-type-of)
   (package :accessor package-of)))

(defmethod available-definer-options ((definer print-object-definer))
  (list #\o #\d))

(defmethod initialize-definer ((definer print-object-definer))
  (prog1 (initialize-function-like-definer
          definer
          (combine-option-writers
           (list
            :print-identity (make-option-writer
                             print-identity-of ensure-boolean-option)
            :print-type (make-option-writer
                         print-type-of ensure-boolean-option)
            :package (make-option-writer package-of))))
    (unless (null (cddr (lambda-list-of definer)))
      (error "Instead of ~s, expecting 2 arguments in the lambda list of ~
              definer ~s of type ~s."
             (lambda-list-of definer) (name-of definer)
             (definer-type definer)))))

(defmethod expand-definer ((definer print-object-definer))
  (with-body-parts (declarations docstring body) (body-of definer)
    `(progn
       (define-print-object
           (,(name-of definer)
            ,@(when (slot-boundp definer 'print-identity)
                `(:identity ,(print-identity-of definer)))
            ,@(when (slot-boundp definer 'print-type)
                `(:type ,(print-type-of definer)))
            ,@(when (slot-boundp definer 'package)
                `(:package ,(package-of definer))))
           ,(lambda-list-of definer)
         ,@(when docstring (list docstring))
         ,@(when-has-option definer #\o (declare-optimize definer))
         ,@(when-has-option definer #\d (declare-debug definer))
         ,@declarations
         ,@body))))


;;; SETF DEFINER ROUTINES

(defclass setf-definer (function-definer)
  ((new-value :accessor new-value-of)))

(defmethod available-definer-options ((definer setf-definer))
  (list #\o #\d))

(defmethod initialize-definer ((definer setf-definer))
  (prog1 (initialize-function-like-definer definer)
    (destructuring-bind (new-value-spec &body body) (body-of definer)
      (unless (and (listp new-value-spec)
                   (null (rest new-value-spec))
                   (symbolp (first new-value-spec)))
        (error "Invalid NEW-VALUE symbol in definer ~s of type ~s."
               (name-of definer) (definer-type definer)))
      (setf (new-value-of definer) (first new-value-spec)
            (body-of definer) body))))

(defmethod expand-definer ((definer setf-definer))
  (with-body-parts (declarations documentation body) (body-of definer)
    `(defsetf ,(name-of definer) ,(lambda-list-of definer)
         (,(new-value-of definer))
       ,@(when documentation `(,documentation))
       ,@(when-has-option definer #\o (declare-optimize definer))
       ,@(when-has-option definer #\d (declare-debug definer))
       ,@declarations
       ,@body)))
