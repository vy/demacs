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

(require 'cl)

(defmacro concatenate-def-types-for-regexp (&rest symbols)
  (with-output-to-string
    (princ "(?\\(")
    (let (not-first)
      (while symbols
        (if not-first
            (princ "\\|")
          (setq not-first t))
        (princ (pop symbols))))
    (princ "\\)")))

(defcustom lisp-indent-def-function-regexp
  (concatenate-def-types-for-regexp
   function macro compiler-macro method generic type print-object class
   condition)
  "Definer types will be indented like a function definition form."
  :type 'string
  :group 'lisp-indent)

(defcustom lisp-indent-def-variable-regexp
  (concatenate-def-types-for-regexp
   variable constant load-time-constant special-variable symbol-macro struct)
  "Definer types will be indented like a variable definition form."
  :type 'string
  :group 'lisp-indent)

(defcustom lisp-indent-def-setf-regexp
  (concatenate-def-types-for-regexp setf)
  "Definer types will be indented like a setf definition form."
  :type 'string
  :group 'lisp-indent)

(defmacro with-position-at-def-type (position &rest body)
  `(save-excursion
     (goto-char ,position)
     (forward-char)
     (forward-sexp 1)
     (while (forward-comment 1))
     ,@body))

(defun lisp-indent-def (path state indent-point sexp-column normal-indent)
  (lisp-indent-259
   (with-position-at-def-type (elt state 1)
     (cond ((looking-at lisp-indent-def-function-regexp)
            '(4 4 &lambda &body))
           ((looking-at lisp-indent-def-variable-regexp)
            '(4 4 &body))
           ((looking-at lisp-indent-def-setf-regexp)
            '(4 4 4 &lambda &body))
           (t
            '(&body))))
   path state indent-point sexp-column normal-indent))

(put 'def 'common-lisp-indent-function 'lisp-indent-def)
