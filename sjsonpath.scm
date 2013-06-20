
(define-module sjsonpath
  (use srfi-1)
  (use srfi-13)
  (use gauche.sequence)
  (use util.match)
  (export sjsonpath json-ref jsonpath->sjsonpath))

(select-module sjsonpath)

(define-class <jsonpath> ()
  ((result-type :init-keyword :arg)
   (root :init-keyword :root)
   (vars :init-keyword :vars)
   (result :init-value '())))

(define (normalize-1 expr)
  (let ((subx '()))
    (values
     (regexp-replace-all #/[\['](\??\(.*?\))[\]']/
       expr
       (^m (push! subx (m 1)) #`"[#,(- (length subx) 1)]"))
     subx)))

(define (normalize-2 expr)
  (regexp-replace-all #/'?\.'?|\['?/ expr ";"))

(define (normalize-3 expr)
  (regexp-replace-all #/;;;|;;/ expr ";..;"))

(define (normalize-4 expr)
  (regexp-replace-all #/;$|'?\]|'$/ expr ""))

(define (normalize-5 expr subx)
  (regexp-replace-all #/#([0-9]+)/
    expr
    (^m (list-ref subx (string->number (m 1))))))

(define (normalize-6 expr)
  (regexp-replace #/^\$;/ expr ""))

(define (normalize expr)
  (receive (expr subx) (normalize-1 expr)
    (let1 r ((compose normalize-4 normalize-3 normalize-2) expr)
      (string-split (normalize-6 (normalize-5 r subx)) #\;))))

(define (%jsonpath->sjsonpath1 loc)
  (cond ((string=? loc "*") '*)
        ((string=? loc "..") '//)
        ((#/,/ loc)
         (cons 'or@
               (map %jsonpath->sjsonpath1 (string-split loc #/'?,'?/))))
        ((#/^\(.*?\)$/ loc)
         (list 'expression@ (read-from-string loc)))
        ((#/^\?(\(.*?\))$/ loc)
         => (^m (list 'filter@ (read-from-string (m 1)))))
        ((#/^(-?[0-9]*):(-?[0-9]*):?([0-9]*)$/ loc)
         => (^m (list 'slice (m 1) (m 2) (m 3))))
        (else loc)))

(define (%jsonpath->sjsonpath expr)
  (if (null? expr)
      '()
      (let1 loc (car expr)
        (cons (%jsonpath->sjsonpath1 loc)
              (%jsonpath->sjsonpath (cdr expr))))))

(define (jsonpath->sjsonpath expr)
  (%jsonpath->sjsonpath (normalize expr)))

(define (asPath path)
  (let1 x (string-split path #\;)
    (call-with-output-string
      (^p
       (display "$" p)
       (for-each
        (^i (display (if-let1 m (#/^[0-9*]+$/ i) #`"[,|i|]" #`"[',|i|']") p))
        x)))))

(define-method store ((self <jsonpath>) (p <string>) v)
  (when p (push! (slot-ref self 'result)
                 (if (eq? (slot-ref self 'result-type) 'path) (asPath p) v)))
  (not (not p)))

(define-method json-ref ((lst <list>) (key <string>))
  (assoc-ref lst key))

(define-method json-ref ((vec <vector>) (key <string>))
  (vector-ref vec (string->number key) #f))

(define-method json-ref ((vec <vector>) (key <integer>))
  (vector-ref vec key #f))

(define-method json-ref (vec key)
  #f)

(define-method recursive-trace
    ((self <jsonpath>) (expr <list>) val loc (path <string>))
  (trace self expr val path)
  (walk loc expr val path
        (^(m l x v p)
          (and (or (list? (json-ref v m)) (vector? (json-ref v m)))
               (trace self (cons '// expr) (json-ref v m) #`",|p|;,|m|")))))

(define-method expression-trace
    ((self <jsonpath>) (expr <list>) val loc (path <string>))
  (trace self
         (cons (x->string (evaluation loc val (string-scan path #\; 'after)))
               expr)
         val
         path))

(define-method filter-trace
    ((self <jsonpath>) (expr <list>) val loc (path <string>))
  (walk loc expr val path
        (^(m l x v p)
          (if (evaluation l (json-ref v m) m)
              (trace self (cons m x) v p)))))

(define-method lambda-filter-trace
    ((self <jsonpath>) (expr <list>) val (loc <procedure>) (path <string>))
  (walk loc expr val path
        (^(m l x v p)
          (if (l (json-ref v m) (slot-ref self 'root) (slot-ref self 'vars))
              (trace self (cons m x) v p)))))

(define-method trace
    ((self <jsonpath>) (expr <list>) val (path <string>))
  (when val
    (if (null? expr)
        (store self path val)
        (receive (loc x)
            (car+cdr expr)
          (match loc
            ('* (walk loc x val path (^(m l x v p)(trace self (cons m x) v p))))
            ('// (recursive-trace self x val loc path))
            (('or@ e ...)
             (for-each (^t (trace self (cons t x) val path)) e))
            (('expression@ e)
             (expression-trace self x val e path))
            (('filter@ e)
             (filter-trace self x val e path))
            (('slice start end step)
             (slice self start end step x val path))
            ((? procedure? e)
             (lambda-filter-trace self x val e path))
            ((? (^x (and (number? x) (vector? val) )) e)
             (trace self x (vector-ref val e #f) #`",|path|;,|loc|"))
            (e (trace self x (json-ref val e) #`",|path|;,|loc|")))))))

(define (walk loc expr val path f)
  (cond ((vector? val)
         (for-each-with-index
          (^(i o) (f (number->string i) loc expr val path))
          val))
        ((list? val)
         (for-each (^i (f (car i) loc expr val path)) val))))

(define-method slice
    ((self <jsonpath>) start end step expr val path)
  (if (vector? val)
      (let ((len (vector-length val)))
        (let ((start (if (string-null? start) 0 (string->number start)))
              (end (if (string-null? end) len (string->number end)))
              (step (if (string-null? step) 1 (string->number step))))
          (let ((start (if (< start 0)
                           (max 0 (+ start len))
                           (min len start)))
                (end (if (< end 0)
                         (max 0 (+ end len))
                         (min len end))))
            (do ((i start (+ i step)))
                ((= i end))
              (trace self (cons (number->string i) expr) val path)))))))

(define (evaluation x v vname)
  (eval `(let ((@ ',v)) ,x) (interaction-environment)))

(define-method sjsonpath ((expr <string>) :optional (vars '()) (arg 'value))
  (sjsonpath (jsonpath->sjsonpath expr) arg vars))

(define-method sjsonpath ((expr <list>) :optional (vars '()) (arg 'value))
  (lambda(obj)
    (let1 jp (make <jsonpath> :arg arg :vars vars :root obj)
      (trace jp expr obj "$")
      (if (null? (slot-ref jp 'result)) #f (reverse! (slot-ref jp 'result))))))
