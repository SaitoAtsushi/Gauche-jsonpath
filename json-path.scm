
(define-module json-path
  (use srfi-13)
  (use gauche.sequence)
  (export json-path json-ref))

(select-module json-path)

(define-class <json-path> ()
  ((result-type :init-keyword :arg)
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
      (normalize-6 (normalize-5 r subx)))))

(define (asPath path)
  (let1 x (string-split path #\;)
    (call-with-output-string
      (^p
       (display "$" p)
       (for-each
        (^i (display (if-let1 m (#/^[0-9*]+$/ i) #`"[,|i|]" #`"[',|i|']") p))
        x)))))

(define-method store ((self <json-path>) (p <string>) v)
  (when p (push! (slot-ref self 'result)
                 (if (eq? (slot-ref self 'result-type) 'path) (asPath p) v)))
  (not (not p)))

(define (chop-head str)
  (receive (head tail)
      (string-scan str ";" 'both)
    (if (and (not head) (not tail))
        (values str "")
        (values head tail))))

(define-method json-ref ((lst <list>) (key <string>))
  (assoc-ref lst key))

(define-method json-ref ((vec <vector>) (key <string>))
  (vector-ref vec (string->number key)))

(define-method json-ref ((vec <vector>) (key <integer>))
  (vector-ref vec key))

(define-method json-ref (vec key)
  #f)

(define-method recursive-trace ((self <json-path>) (expr <string>) val (loc <string>) (path <string>))
  (trace self expr val path)
  (walk loc expr val path
        (^(m l x v p)
          (and (or (list? (json-ref v m)) (vector? (json-ref v m)))
               (trace self #`"..;,|expr|" (json-ref v m) #`",|p|;,|m|")))))

(define-method expression-trace ((self <json-path>) (expr <string>) val (loc <string>) (path <string>))
  (trace self
         (let1 t (evaluation loc val (string-scan path #\; 'after))
           #`",|t|;,|expr|")
         val
         path))

(define-method filter-trace ((self <json-path>) (expr <string>) val (loc <string>) (path <string>))
  (walk loc expr val path
        (^(m l x v p)
          (if (evaluation
               (regexp-replace #/^\?\((.*?)\)$/ l "(\\1)")
               (json-ref v m)
               m)
              (trace self #`",|m|;,|x|" v p)))))
  
(define-method trace ((self <json-path>) (expr <string>) val (path <string>))
  (if (string-null? expr)
      (store self path val)
      (receive (loc x)
          (chop-head expr)
        (cond ((and (list? val) (assoc loc val))
               (trace self x (json-ref val loc) #`",|path|;,|loc|"))
              ((and (vector? val) (string->number loc))
               (trace self x (json-ref val loc) #`",|path|;,|loc|"))
              ((and (string? loc) (string=? loc "*"))
               (walk loc x val path
                     (^(m l x v p) (trace self #`",|m|;,|x|" v p))))
              ((string=? loc "..")
               (recursive-trace self x val loc path))
              ((#/,/ loc)
               (let1 s (string-split loc #/'?,'?/)
                 (for-each (^t (trace self #`",|t|;,|x|" val path)) s)))
              ((#/^\(.*?\)$/ loc)
               (expression-trace self x val loc path))
              ((#/^\?\(.*?\)$/ loc)
               (filter-trace self x val loc path))
              ((#/^(-?[0-9]*):(-?[0-9]*):?([0-9]*)$/ loc)
               (slice self loc x val path))))))

(define (walk loc expr val path f)
  (cond ((vector? val)
         (for-each-with-index
          (^(i o) (f (number->string i) loc expr val path))
          val))
        ((list? val)
         (for-each (^i (f (car i) loc expr val path)) val))))

(define-method slice ((self <json-path>) loc expr val path)
  (if (vector? val)
      (let ((len (vector-length val)))
        (regexp-replace-all #/^(-?[0-9]*):(-?[0-9]*):?(-?[0-9]*)$/
          loc
          (^m (let ((start (if (string-null? (m 1)) 0 (string->number (m 1))))
                    (end (if (string-null? (m 2)) len (string->number (m 2)))))
                (let ((start (if (< start 0)
                                 (max 0 (+ start len))
                                 (min len start)))
                      (end (if (< end 0)
                               (max 0 (+ end len))
                               (min len end))))
                  (do ((i start (+ i 1)))
                      ((= i end))
                    (trace self #`",|i|;,|expr|" val path)))))))))

(define (evaluation x v vname)
  (eval `(let ((@ ',v)) ,(read-from-string x)) (interaction-environment)))

(define (json-path obj expr :optional (arg 'value))
  (let1 jp (make <json-path> :arg arg)
    (trace jp (normalize expr) obj "$")
    (if (null? (slot-ref jp 'result)) #f (reverse! (slot-ref jp 'result)))))
