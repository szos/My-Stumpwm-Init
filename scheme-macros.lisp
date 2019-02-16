;;mbe.lsp - now called scheme-macros.lisp, and integrated into stumpwm.
;;Dorai Sitaram
;;April 16, 1998
;;v. 0a

;; this file is licensed under the LGPL, as per https://ds26gte.github.io/index.html

;;Implementation of R5RS-style define-syntax, let-syntax and
;;letrec-syntax for Common Lisp.

;;This uses `***' as the ellipsis indicator, since Common
;;Lisp won't allow `...' as an identifier.  Change
;;*schemembe-ellipsis-indicator* if you don't like `***'.
(in-package :stumpwm)

(defconstant *schemembe-ellipsis-indicator* '***)

(defun schemembe-symbol? (x)
  (and (symbolp x)
       (not (or (eq x t) (eq x nil)))))

(defun schemembe-list? (x)
  (and (listp x) (null (cdr (last x)))))

(defun schemembe-ellipsis? (x)
  (and (consp x) (consp (cdr x)) (eq (cadr x) *schemembe-ellipsis-indicator*)))

(defun schemembe-matches-pattern? (p e k)
  (cond ((schemembe-ellipsis? p)
         (if (not (= (list-length p) 2)) (error "bad ellipsis: ~a" p))
         (and (schemembe-list? e)
              (let ((p0 (car p)))
                (every #'(lambda (e_i) (schemembe-matches-pattern? p0 e_i k))
                       e))))
        ((consp p)
         (and (consp e) (schemembe-matches-pattern? (car p) (car e) k)
              (schemembe-matches-pattern? (cdr p) (cdr e) k)))
        ((schemembe-symbol? p) (if (member p k) (eq p e) t))
        (t (equal p e))))

(defun schemembe-get-ellipsis-nestings (p k)
  (labels ((sub (p)
             (cond ((schemembe-ellipsis? p)
                    (cons (sub (car p)) (sub (cddr p))))
                   ((consp p) (nconc (sub (car p)) (sub (cdr p))))
                   ((schemembe-symbol? p) (if (member p k) '() (list p)))
                   (t '()))))
    (sub p)))

(defun schemembe-ellipsis-sub-envs (nestings r)
  (some #'(lambda (c)
            (if (schemembe-intersect? nestings (car c)) (cdr c) nil))
        r))

(defun schemembe-intersect? (v y)
  (if (or (schemembe-symbol? v) (schemembe-symbol? y))
      (eq v y)
    (some #'(lambda (v_i)
              (some #'(lambda (y_j) (schemembe-intersect? v_i y_j)) y))
          v)))

(defun schemembe-get-bindings (p e k)
  (cond ((schemembe-ellipsis? p)
         (let ((p0 (car p)))
           (list (cons (schemembe-get-ellipsis-nestings p0 k)
                       (mapcar #'(lambda (e_i)
                                   (schemembe-get-bindings p0 e_i k))
                               e)))))
        ((consp p)
         (nconc (schemembe-get-bindings (car p) (car e) k)
                (schemembe-get-bindings (cdr p) (cdr e) k)))
        ((schemembe-symbol? p) (if (member p k) '() (list (cons p e))))
        (t '())))

(defun schemembe-expand-pattern (p r k)
  (cond ((schemembe-ellipsis? p)
         (nconc (let* ((p0 (car p))
                       (nestings (schemembe-get-ellipsis-nestings p0 k))
                       (rr (schemembe-ellipsis-sub-envs nestings r)))
                  (mapcar #'(lambda (r_i)
                              (schemembe-expand-pattern p0 (append r_i r) k))
                          rr))
                (schemembe-expand-pattern (cddr p) r k)))
        ((consp p)
         (cons (schemembe-expand-pattern (car p) r k)
               (schemembe-expand-pattern (cdr p) r k)))
        ((schemembe-symbol? p)
         (if (member p k) p (let ((x (assoc p r))) (if x (cdr x) p))))
        (t p)))

(defun schemembe-syntax-rules-proc (macro-name kk cc arg-sym kk-sym)
  (let ((kk (cons macro-name kk)))
    `(let ((,arg-sym (cons ',macro-name ,arg-sym))
           (,kk-sym ',kk))
       (cond ,@(mapcar #'(lambda (c)
                           (let ((in-pat (car c)) (out-pat (cadr c)))
                             `((schemembe-matches-pattern?
                                ',in-pat ,arg-sym ,kk-sym)
                               (let ((r (schemembe-get-bindings ',in-pat
                                                                ,arg-sym
                                                                ,kk-sym)))
                                 ,(if (and (consp out-pat)
                                           (eq (car out-pat) 'with))
                                      `(schemembe-expand-pattern
                                        ',(caddr out-pat)
                                        (nconc (list ,@(mapcar #'(lambda (w)
                                                                   `(cons ',(car w)
                                                                          ,(cadr w)))
                                                               (cadr out-pat)))
                                               r)
                                        ,kk-sym)
                                    `(schemembe-expand-pattern ',out-pat r
                                                               ,kk-sym))))))
                       cc)
             (t (error "~a: no matching clause" ',macro-name))))))
                       
(defmacro define-syntax (macroname synrules)
  (let ((keywords (cadr synrules))
        (clauses (cddr synrules)))
    `(defmacro ,macroname (&rest __syntax-rules-arg__)
       ,(schemembe-syntax-rules-proc macroname keywords clauses
                                     '__syntax-rules-arg__
                                     '__syntax-rules-keywords__))))

(defmacro letrec-syntax (synruledefs &rest body)
  `(macrolet
    ,(mapcar #'(lambda (synruledef)
                 (let ((macroname (car synruledef))
                       (keywords (cadadr synruledef))
                       (clauses (cddadr synruledef)))
                   `(,macroname (&rest __syntax-rules-arg__)
                                ,(schemembe-syntax-rules-proc macroname
                                                              keywords
                                                              clauses
                                                              '__syntax-rules-arg__
                                                              '__syntax-rules-keywords__))))
             synruledefs)
    ,@body))

;Actually, CL can't distinguish let- from letrec-syntax very well.

(defmacro let-syntax (synruledefs &rest body)
  (case (length synruledefs)
        ((0) `(progn ,@body))
        ((1) `(letrec-syntax ,synruledefs ,@body))
        (otherwise
         `(letrec-syntax (,(car synruledefs))
                         (let-syntax ,(cdr synruledefs)
                                     ,@body)))))

;end of file




