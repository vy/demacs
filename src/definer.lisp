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


;;; ABSTRACT DEFINER DECLARATIONS

(defclass definer ()
  ((name
    :initarg :name
    :accessor name-of)
   (options
    :initarg :definer-options
    :accessor options-of)
   (forms
    :initarg :forms
    :accessor forms-of))
  (:documentation "Generic class holding definer meta information."))

(define-print-object (definer :identity nil) (self stream)
  (format stream "definer ~s" (name-of self)))

(defun get-definer (name)
  "Returns definer class for the supplied definer of NAME."
  (or (find-class (intern (format nil "~:@(~a-definer~)" name)) :errorp nil)
      (error "Unknown definer: `~a'." name)))

(defgeneric available-definer-options (definer)
  (:documentation "List of available options for related definer."))

(defgeneric restricted-definer-options (definer)
  (:documentation
   "List of restricted option combinations for related definer."))

(defgeneric initialize-definer (definer)
  (:documentation "Initializes related definer class slots."))

(defgeneric expand-definer (definer)
  (:documentation "Expands related definer class into its compilation form."))

(defmacro def (name-and-options &rest rest)
  (destructuring-bind (name &rest definer-options) (ensure-list name-and-options)
    (expand-definer
     (initialize-definer (make-instance (get-definer name)
                                        :definer-options definer-options
                                        :forms rest)))))

;;; COMMON ROUTINES

(defun definer-type (definer)
  (class-name (class-of definer)))

(defun ensure-boolean-option (definer keyword value)
  (unless (typep value 'boolean)
    (error "Expecting a boolean instead of ~s for ~s in definer ~s of type ~s."
           value keyword (name-of definer) (definer-type definer)))
  value)

(defun ensure-string-option (definer keyword value)
  (unless (stringp value)
    (error "Expecting a string instead of ~s for ~s in definer ~s of type ~s."
           value keyword (name-of definer) (definer-type definer)))
  value)

(defun ensure-function-option (definer keyword value)
  (declare (ignore definer keyword))
  (if (and (consp value) (member (car value) '(function lambda)))
      value
      `(fdefinition ,value)))

(defun oerror (fmt options definer &rest rest)
  (apply #'error fmt options (name-of definer)
         (definer-type definer) rest))

(defun validate-definer-options (definer &optional extra-options-writer)
  "Validates definer options of function like definers."
  (destructuring-bind (options extra-options)
      ;; Extract definer options.
      (let* ((options (options-of definer))
             (probable-options (coerce (symbol-name (car options)) 'list))
             (available-options (available-definer-options definer)))
        (if (set-difference
             probable-options available-options :test #'char-equal)
            (list nil options)
            (list probable-options (rest options))))
    ;; Check extra options.
    (when (and extra-options (not extra-options-writer))
      (oerror "Invalid definer options ~s in definer ~s of type ~s."
              extra-options definer))
    ;; Check restricted options.
    (dolist (restricted-combination (restricted-definer-options definer))
      (when (every (lambda (option) (member option options :test #'char-equal))
                   restricted-combination)
        (oerror "Ambiguous definer options ~s in definer ~s of type ~s. ~
                 (Cannot use ~s definer options at the same.)"
                options definer restricted-combination)))
    ;; Update validated slot values.
    (setf (options-of definer) options)
    (when extra-options-writer
      (funcall extra-options-writer definer extra-options))))

(defun combine-option-writers (option-writers)
  (lambda (definer options)
    ;; Apply each OPTION-WRITER-FUNCTION, if found appropriate keyword in the
    ;; options of related definer.
    (loop with no-value-p = (gensym)
          for (keyword writer-function) on option-writers by #'cddr
          for option = (getf options keyword no-value-p)
          unless (eql option no-value-p)
          do (progn
               (funcall writer-function definer keyword option)
               (remf options keyword)))
    ;; Check whether we processed all options.
    (unless (null options)
      (oerror "Invalid options ~s for definer ~s of type ~s."
              options definer))))

(defmacro make-option-writer (slot-writer &optional validator)
  (with-unique-names (definer keyword value)
    `(lambda (,definer ,keyword ,value)
       ,@(unless validator
           `((declare (ignore ,keyword))))
       (setf (,slot-writer ,definer)
             ,(if validator
                  `(,validator ,definer ,keyword ,value)
                  value)))))

(defun has-option-p (definer option)
  (member option (options-of definer) :test #'char-equal))
