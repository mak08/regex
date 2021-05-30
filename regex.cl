;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2021-05-16 22:22:20>

;;; (declaim (optimize (debug 3) (speed 0) (safety 0) (space 0)))
         
(in-package regex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defmacro memoized (arg memo-ht form)
  `(multiple-value-bind (value found)
       (gethash ,arg ,memo-ht)
     (cond (found value)
           (T
            (setf (gethash ,arg ,memo-ht)
                  ,form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface functions

(defun make-regex (expr)
  (compile-regex
   (parse-regex expr)))

(define-compiler-macro make-regex (&whole form &rest args)
  (typecase (car args)
    (string
     (compile-regex
      (parse-regex (car args))))
    (otherwise
     form)))

(defun match-regex (nfa string &key (start 0))
  (let ((state-ht (nfa-state-ht nfa))
        (delta-ht (nfa-delta-ht nfa))
        (eps-ht (nfa-eps-ht nfa))
        (accept-ht (nfa-accept-ht nfa))
        (len (length string)))
    (declare (special state-ht eps-ht delta-ht))
    (flet ((accepting (state)
             (member (nfa-accepting nfa) state)))
      (loop
         :with cur-state = (nfa-initial nfa)
         :with cur-accept = ()
         :for cur-pos  :from start
         :for cur-char = (when (< cur-pos len) (aref string cur-pos))
         :do (progn
               (when cur-char
                 (setf cur-state (next-state cur-state cur-char nfa)))
               (when (or (not cur-char)    ; at EOS
                         (null cur-state)) ; no transition on input
                 ;; If this assert fails, set lexstream-pos here and implement ls-peek accordingly
                 ;; (assert (eql  (lexstream-position lexstream) cur-pos))
                 (return-from match-regex
                   (and (not cur-char)
                        (accepting cur-state)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse tree constructors

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass node-1 ()
  ((arg :accessor arg :initarg :arg)))

(defclass node-2 ()
  ((arg1 :accessor arg1 :initarg :arg1)
   (arg2 :accessor arg2 :initarg :arg2)))

(defmethod print-object ((node node-1) stream)
  (format stream "#<~s ~s>"
          (class-name (class-of node))
          (arg node)))

(defmethod print-object ((node node-2) stream)
  (format stream "#<~s ~s ~s>"
          (class-name (class-of node))
          (arg1 node)
          (arg2 node)))

(defclass regexp (node-1) ())
(defclass concat (node-1) ())
(defclass choice (node-1) ())
(defclass range (node-2) ())
(defclass repeat (node-1) ())
(defclass wildchar (node-1) ())
(defclass optional (node-1) ())

(defun regex (symbol tree level)
  tree)

(defun subregex (symbol tree level)
  (cadr tree))
  
(defun concat (symbol tree level)
  (typecase tree
    (atom
     tree)
    (otherwise
     (if (typep (cadr tree) 'concat)
         (progn
           (push (car tree) (arg (cadr tree)))
           (cadr tree))
         (make-instance 'concat :arg (list (car tree) (cadr tree)))))))

(defun choice (symbol tree level)
  (typecase tree
    (atom
     tree)
    (otherwise
     (if (typep (caddr tree) 'choice)
         (progn
           (push (car tree) (arg (caddr tree)))
           (caddr tree))
         (make-instance 'choice :arg (list (car tree) (caddr tree)))))))

(defun repeat (symbol tree level)
  tree)

(defun repeat* (symbol tree level)
  (make-instance 'repeat :arg (car tree)))

(defun wildchar (symbol tree level)
  (make-instance 'wildchar :arg "."))

(defun optional (symbol tree level)
  (make-instance 'optional :arg (car tree)))

(defun factor (symbol tree level)
  tree)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
;;;
;;;    Fixme: defparser generates a very inefficient parser.

(defcharbag :nonspecial "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-/:=")
(defcharbag :special "\\.?*+()[]|")

(defparser parse-regex
    :rules ((regex
             choice))

    :tokens ((choice
              (:alt (:seq concat "|" choice)
                    concat))
             (concat
              ;; repeat may not derive \eps, otherwise the concat rule will loop forever!
              (:alt (:seq repeat concat)
                    repeat))
             (repeat
              (:alt repeat*
                    optional
                    factor))
             (repeat*
              (:seq factor "*"))
             (optional
              (:seq factor "?"))
             (factor
              (:alt literal
                    wildchar
                    subregex))
             (wildchar
              ".")
             (literal
              (:alt (:seq "\\" (:alt :special :nonspecial))
                    :nonspecial))
             (subregex
              (:seq "(" choice ")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiler

(defstruct (nfa (:print-object print-nfa))
  (initial ())
  (accepting ())
  (transitions (make-hash-table :test #'eq))
  (wild-transitions (make-hash-table :test #'eq))
  (state-ht (make-hash-table :test #'equal))
  (delta-ht (make-hash-table :test #'equal))
  (eps-ht (make-hash-table :test #'equal))
  (accept-ht (make-hash-table :test #'equal)))

(defun print-nfa (nfa stream)
  (format stream "#<NFA ~a ~a>" (nfa-initial nfa) (nfa-accepting nfa)))

(defun add-transition (source target symbol nfa)
  (let* ((transition-table (nfa-transitions nfa))
         (source-ht (or (gethash source transition-table)
                       (setf (gethash source transition-table)
                         (make-hash-table :test #'eq)))))
    (pushnew target (gethash symbol source-ht ()))))

(defun add-wild-transition (source target nfa)
  (let* ((transition-table (nfa-wild-transitions nfa))
         (source-ht (or (gethash source transition-table)
                       (setf (gethash source transition-table)
                         (make-hash-table :test #'eq)))))
    (pushnew target (gethash :wild source-ht ()))))

(defun compile-regex (regex)
  (let* ((nfa (make-nfa))
         (source (gensym "start-"))
         (target (gensym "accept-"))
         (eps-ht (nfa-eps-ht nfa))
         (delta-ht (nfa-delta-ht nfa)))
    (declare (special delta-ht eps-ht))
    (compile-regexp% regex source target nfa)
    (setf (nfa-initial nfa) (union (list source)
                                   (eps-delta (list source)
                                              (nfa-transitions nfa))))
    (setf (nfa-accepting nfa) target)
    nfa))

(defun token-char (token)
  (ecase (length (token-value token))
    (1
     (aref (token-value token) 0))
    (2
     (aref (token-value token) 1))))

;; NIL is regarded a token of type NULL
(defmethod token-type ((thing (eql NIL)))
  '(null))
(defmethod token-value ((thing (eql NIL)))
  "")

(defmethod compile-regexp% ((token T) source target nfa)
  (add-transition source target (token-char token) nfa))

(defmethod compile-regexp% ((token wildchar) source target nfa)
  (add-wild-transition source target nfa))

(defmethod compile-regexp% ((regexp choice) source target nfa)
  (mapcar (lambda (alt)
            (let ((in (gensym "IN-"))
                  (out (gensym "OUT-")))
              (compile-regexp% alt in out nfa)
              (add-transition source in () nfa)
              (add-transition out target () nfa)))
          (arg regexp)))

(defmethod compile-regexp% ((regexp repeat) source target nfa)
  (let ((in (gensym "IN-"))
        (out (gensym "OUT-")))
    (compile-regexp% (arg regexp) in out nfa)
    (add-transition source target () nfa)
    (add-transition out in () nfa)
    (add-transition source in () nfa)
    (add-transition out target () nfa)))

(defmethod compile-regexp% ((regexp optional) source target nfa)
  (compile-regexp% (arg regexp) source target nfa)
  (add-transition source target () nfa))

(defmethod compile-regexp% ((regexp concat) source target nfa)
  (let ((state-1)
        (state-2 source))
    (dolist (reg-i (arg regexp))
      (shiftf state-1 state-2 (gensym "CONC-"))
      (compile-regexp% reg-i state-1 state-2 nfa))
    (add-transition state-2 target () nfa)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NFA Implementation

(defun next-state (current input nfa)
  (declare (special state-ht))
  (memoized (cons input current) state-ht
            (let ((next-state
                   ;; transition on input
                   (union (delta current input (nfa-transitions nfa))
                          (delta current :wild (nfa-wild-transitions nfa)))))
              ;; epsilon moves
              (union (eps-delta next-state (nfa-transitions nfa))
                     next-state))))

(defun delta (cur-state symbol transitions)
  (declare (special delta-ht))
  (memoized (cons symbol cur-state) delta-ht
            (delete-duplicates
             (loop
               :for state in cur-state
               :for trans = (gethash state transitions)
               :append (when trans (gethash symbol trans))))))

(defun eps-delta (cur-state transitions)
  (declare (special eps-ht))
  (memoized cur-state eps-ht
            (do ((eps-reachable ())
                 (eps-next cur-state
                           (set-difference (delta eps-next () transitions)
                                           eps-reachable)))
                ((null eps-next) eps-reachable)
              (setf eps-reachable (append eps-next eps-reachable)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Performance
;;;
;;; (setf r (make-regex "((ab|ba)*|(ba|ab)*)*"))
;;; (time (dotimes (k 100000) (match-regex r "baabababbababaabbaababbaabba")))
;;; | Evaluation took:
;;; |   0.373 seconds of real time
;;; |   0.376128 seconds of total run time (0.376128 user, 0.000000 system)
;;; |   [ Run times consist of 0.015 seconds GC time, and 0.362 seconds non-GC time. ]
;;; |   100.80% CPU
;;; |   895,776,022 processor cycles
;;; |   60,779,408 bytes consed
;;; 2016-02-17 22:24:17 mka
;;; | Evaluation took:
;;; |   0.340 seconds of real time
;;; |   0.350025 seconds of total run time (0.350025 user, 0.000000 system)
;;; |   102.94% CPU
;;; |   814,554,900 processor cycles
;;; |   44,793,856 bytes consed

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
