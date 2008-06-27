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

(defmacro when-let ((var test) &body body)
  `(let ((,var ,test))
     (when ,var ,@body)))

(defun ensure-function (form)
  (if (and (consp form) (member (car form) '(function lambda)))
      form
      `(fdefinition ,form)))

;;; Copied from lists.lisp of alexandria project. (See
;;; http://common-lisp.net/project/alexandria/)
(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by
LIST."
  (if (listp list)
      list
      (list list)))

;;; Copied from cl-ppcre project of Dr. Edmund Weitz. Reference implementation
;;; posted to comp.lang.lisp as <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa.
#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

(defun parse-body-parts (body)
  "Parses supplied BODY with respect to ([[declaration* | documentation]] form*)
pattern and returns list of DECLARATIONS, DOCUMENTATION and FORM values."
  (loop with declarations
	with documentation
	for forms on body
	for form = (first forms)
	while forms
        do (let ((form (car forms)))
             (cond ((and (listp form)
                         (eql (car form) 'declare))
                    (push form declarations))
                   ((and (stringp form)
                         (null documentation)
                         (cdr forms))
                    (setq documentation form))
                   (t (loop-finish))))
	finally (return (list (nreverse declarations) documentation forms))))

(defmacro with-body-parts ((declarations docstring rest) target-body &body body)
  "Binds passed DECLARATIONS, DOCSTRING and REST to the related parts of the
TARGET-BODY."
  `(destructuring-bind (,declarations ,docstring ,rest)
       (parse-body-parts ,target-body)
     ,@body))

(defmacro define-print-object ((class &key (identity t) (type t) package)
                               (self &optional stream) &body body)
  "Define a PRINT-OBJECT method using PRINT-UNREADABLE-OBJECT."
  (let ((stream-symbol (or stream (gensym))))
    (with-body-parts (declarations docstring body) body
      `(defmethod print-object ((,self ,class) ,stream-symbol)
         ,@declarations
         ,@(when docstring (list docstring))
         (print-unreadable-object
             (,self ,stream-symbol :type ,type :identity ,identity)
           (let (,@(unless stream `((*standard-output* ,stream-symbol)))
                 ,@(when package `((*package ,(find-package package)))))
             ,@body))))))
