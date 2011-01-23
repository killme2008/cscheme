(ns scheme.core)
;;预定义car cdr原语

(def car first)
(def cdr next)
(defn cadr [exp] (car (cdr exp)))
(defn caadr [exp] (car (car (cdr exp))))
(defn caddr [exp] (car (cdr (cdr exp))))
(defn cdddr [exp] (cdr (cdr (cdr exp))))
(defn cadddr [exp] (car (cdr (cdr (cdr exp)))))
(defn tagged-list? [exp sym]
  (if (seq? exp)
      (= (car exp) sym)
      false))
;;环境
(def the-empty-environment '())

(defn first-frame [env]
  (car env))

(defn enclosing-environment [env]
  (cdr env))

(defn make-frame [variables values]
  (let [lists (map list variables values)]
     (transient (reduce #(assoc %1 (keyword (str (car %2))) (cadr %2)) (hash-map) lists))))

(defn add-binding-to-frame! [variable value frame]
    (assoc! frame (keyword (str variable)) value))

(defn extend-environment [variables values base-env] 
  (if (= (.size variables) (.size values))
    (cons (make-frame variables values) base-env)
    (if (< (.size variables) (.size values))
      (throw (IllegalArgumentException. "Too many arguments supplied"))
      (throw (IllegalArgumentException. "Too few arguments supplied")))))

(defn lookup-variable-value [variable env]
  (defn scan [variable frame env]
    (if (nil? frame)
      (throw (Exception. (str "Unbound variable " variable)))
      (let [value ((keyword (str variable)) frame)]
        (if (nil? value)
          (recur variable (car env) (enclosing-environment  env))
          value))))
  (scan variable (first-frame env) (enclosing-environment env)))

(defn set-variable-value! [variable value env]
  (defn scan [variable frame env]
    (if (nil? frame)
      (throw (Exception. (str "Unbound variable " variable)))
      (let [v ((keyword (str variable)) frame)]
        (if (nil? v)
          (recur variable (car env) (enclosing-environment  env))
          (assoc! frame (keyword (str variable)) value)))))
  (scan variable (first-frame env) (enclosing-environment env)))

(defn define-variable! [variable value env]
  (defn scan [variable frame env] 
      (let [v ((keyword (str variable)) frame)]
        (if (nil? v)
            (add-binding-to-frame! variable value frame)
            (assoc! frame (keyword (str variable)) value))))
  (scan variable (first-frame env) (enclosing-environment env)))

(defn primitive-procedure? [p]
  (tagged-list? p 'primitive))

(defn primitive-implementation [p]
  (cadr p))

(def *primitive-procedures*
  (list (list 'car car) 
        (list 'cdr cdr)
        (list 'list list)
        (list 'cons cons)
        (list 'null? nil?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'assoc assoc)
        (list 'cadr cadr)
        (list 'cadr caddr)
        (list 'display print)
        (list 'newline newline)
        (list 'map map)))

(def *primitive-procedure-names*
  (map car *primitive-procedures*))

(def *primitive-procedure-objects*
  (map #(list 'primitive (cadr %)) *primitive-procedures*))

(defn setup-environment []
  (let [initial-env (extend-environment *primitive-procedure-names* *primitive-procedure-objects* the-empty-environment)]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(def *the-global-environment* (setup-environment))

(defn apply-primitive-procedure [proc args]
  (apply (primitive-implementation proc) args))


;;常量
(defn self-evaluting? [exp]
  (cond 
    (string? exp) true
    (number? exp) true
    :else
    false))
(defn analyze-self-evaluting [exp]
  (fn [env] exp))
;;变量
(defn variable? [exp]
  (or (symbol? exp) (= true exp) (= false exp)))
;;quote
(defn quoted? [exp] (tagged-list? exp 'quote))
(defn text-of-quotation [exp]
  (cadr exp))

;;赋值
(defn assignment? [exp]
  (tagged-list? exp 'set!))

(defn assignment-variable [exp]
  (cadr exp))
(defn assignment-value [exp]
  (caddr exp))

;;lambda?
(defn lambda? [exp]
  (tagged-list? exp 'lambda))
(defn lambda-parameters [exp]
  (cadr exp))
(defn lambda-body [exp]
  (cdr (cdr exp)))
(defn make-lambda [params body]
  (cons 'lambda (cons params body)))

;;定义
(defn definition? [exp]
  (tagged-list? exp 'define))
(defn definition-variable [exp]
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(defn definition-value [exp]
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdr (cadr exp)) (cdr (cdr exp)))))
;;if语句
(defn if? [exp]  (tagged-list? exp 'if)) 
(defn if-predicate [exp] (cadr exp))
(defn if-then [exp] (caddr exp))
(defn if-else [exp] 
  (if ((comp not nil?) (cdddr exp))
      (cadddr exp)
      'false))
(defn make-if [predicate then else]
  (list 'if predicate then else))

;;begin sequence
(defn begin? [exp] 
  (tagged-list? exp 'begin))

(defn begin-actions [exp] 
  (cdr exp))
(defn last-exp? [exp]
  (nil? (cdr exp)))
(defn first-exp [exp]
  (car exp))
(defn rest-exps [exps]
  (cdr exps))
(defn make-begin [exps]
  (list 'begin exps))
(defn sequence->exp
  [exps]
  (cond 
    (nil? exps) exps
    (last-exp? exps) (first-exp exps)
    :else
    (make-begin exps)))
;;application
(defn application? [exp]
  (seq? exp))
(defn operator [exp]
  (car exp))
(defn operands [exp]
  (cdr exp))
(defn no-operands? [ops]
  (nil? ops))
(defn first-operand [ops]
  (car ops))
(defn rest-operands [ops]
  (cdr ops))
;;let?
(defn let? [exp]
  (tagged-list? exp 'let))
(defn make-define [variable parameters body]
  (list 'define (cons variable parameters) body))
(defn let->combination [exp]
  (if (symbol? (cadr exp))
    (let [variable (cadr exp)
          variables (map car (caddr exp))
          values (map cadr (caddr exp))
          pairs (caddr exp)
          body (cadddr exp)]
      (cons (make-lambda variables (list (make-define variable variables body) body)) values))
    (let [variables (map car (cadr exp))
          values (map cadr (cadr exp))
          body (caddr exp)]
              (cons (make-lambda variables (list body)) values))))
;;cond
(defn cond? [exp]
  (tagged-list? exp 'cond))
(defn cond-clauses [exp]
  (cdr exp))
(defn cond-extended-clauses? [clause]
  (and (> (count clause) 2) (= (cadr clause) '=>)))
(defn extended-cond-test [clause]
  (car clause))
(defn extended-cond-recipient [clause]
  (caddr clause)) 
(defn cond-predicate [clause] (car clause))
(defn cond-actions [clause] (cdr clause))

(defn cond-else-caluses? [exp]
  (= (cond-predicate exp) 'else))

(defn expand-clauses [exp]
  (if (nil? exp)
    'false
    (let [f (car exp)
          r (cdr exp)]
      (cond 
        (cond-else-caluses? f) (if (nil? r)
                                 (sequence->exp (cond-actions f))
                                 (throw (Exception. "else clause is not LAST")))
        (cond-extended-clauses? f) (make-if (extended-cond-test f)
                                     (list (extended-cond-recipient f)
                                           (extended-cond-test f)
                                           (expand-clauses r)))
        :else
        (make-if (cond-predicate f) (sequence->exp (cond-actions f)) (expand-clauses r))))))

 (defn cond->if [exp]
   (expand-clauses (cond-clauses exp)))
 ;;procedure
 (defn make-procedure [params body env]
   (list 'procedure params body env))
 (defn compound-procedure? [p]
   (tagged-list? p 'procedure))
 (defn procedure-parameters [p]
   (cadr p))
 (defn procedure-body [p]
   (caddr p))
 (defn procedure-environment [p]
   (cadddr p))

;;analyze procedure
(declare analyze)

(defn analyze-self-evaluting [exp]
  (fn [env] exp))
(defn analyze-variable [exp]
  (fn [env] (lookup-variable-value exp env)))
(defn analyze-quotation [exp]
  (fn [env] (text-of-quotation exp)))
(defn analyze-assignment [exp]
  (let [variable (assignment-variable exp)
        vproc (analyze (assignment-value exp))]
    (fn [env] (set-variable-value! variable (vproc env) env) 'ok)))
(defn analyze-definition [exp]
  (let [variable (definition-variable exp)
        vproc (analyze (definition-value exp))]
    (fn [env] (define-variable! variable  (vproc env) env) 'ok)))
(defn analyze-if [exp]
  (let [pproc (analyze (if-predicate exp))
        tproc (analyze (if-then exp))
        eproc (analyze (if-else exp))]
    (fn [env]
      (if (true? (pproc env))
        (tproc env)
        (eproc env)))))
(defn analyze-sequence [exps]
  (defn sequentially [proc1 proc2]
    (fn [env] (proc1 env) (proc2 env)))
  (defn loop-seq [first-proc rest-proc]
    (if (nil? rest-proc)
      first-proc
      (recur (sequentially first-proc (car rest-proc)) (cdr rest-proc))))
  (let [procs (map analyze exps)]
    (if (nil? procs)
      (throw (Exception. "Empty sequences"))
      (loop-seq (car procs) (cdr procs)))))
(defn analyze-lambda [exp]
  (let [variables (lambda-parameters exp)
        bproc (analyze-sequence (lambda-body exp))]
    (fn [env]
      (make-procedure variables bproc env))))
(defn execution-application [proc args]
  (cond 
    (primitive-procedure? proc) (apply-primitive-procedure proc args)
    (compound-procedure? proc) ((procedure-body proc) (extend-environment (procedure-parameters proc)
                                                        args
                                                        (procedure-environment proc)))
    :else
    (throw (Exception. "Unknown procedure type"))))
(defn analyze-application [exp]
  (let [fproc (analyze (operator exp))
        aprocs (map analyze (operands exp))]
    (fn [env]
      (execution-application (fproc env)
        (map #(% env) aprocs)))))

        

(defn analyze
  [exp]
  (cond 
    (self-evaluting? exp) (analyze-self-evaluting exp)
    (variable? exp) (analyze-variable exp)
    (quoted? exp) (analyze-quotation exp)
    (assignment? exp) (analyze-assignment exp)
    (definition? exp) (analyze-definition exp)
    (if? exp) (analyze-if exp)
    (lambda? exp) (analyze-lambda exp)
    (begin? exp) (analyze-sequence (begin-actions exp))
    (cond? exp) (recur (cond->if exp))
    (let? exp) (recur (let->combination exp))
    (application? exp) (analyze-application exp)
    :else
    (throw (Exception. (str "Unknow expression type" exp)))))

;;eval
(defn scheme-eval [exp env]
  (try
    ((analyze exp) env)
   (catch Exception e (.getMessage e))))


;;解释器
(def input-prompt "user=>")
(def out-prompt "")
(defn prompt-for-input [s]
  (println s))
(defn announce-output [s]
  (print s))
(defn user-print [object]
  (if (compound-procedure? object)
      (print (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (do (print object) (newline))))
(defn drive-loop []
  (prompt-for-input input-prompt)
  (let [input (read)]
    (let [output (scheme-eval input *the-global-environment*)]
      (announce-output out-prompt)
      (user-print output)))
  (recur))