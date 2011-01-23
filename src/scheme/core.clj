(ns scheme.core)
;;预定义car cdr原语

(def car first)
(def cdr rest)
(defn cadr [exp] (car (cdr exp)))
(defn caddr [exp] (car (cdr (cdr exp))))
(defn cadddr [exp] (car ((cdr (cdr (cdr exp))))))
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
     (transient (reduce #(assoc %1 (keyword (str (car %2))) (car (cadr %2))) (hash-map) lists))))

(defn add-binding-to-frame! [variable value frame]
    (println (keyword (str variable)) value)
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
  (tagged-list? p 'procedure))

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
  (map (fn [proc] (list 'primitive (cadr proc))) *primitive-procedures*))

(defn setup-environment []
  (let [initial-env (extend-environment *primitive-procedure-names* *primitive-procedure-objects* the-empty-environment)]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(def *the-global-environment* (setup-environment))

(defn apply-primitive-procedure [proc & args]
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
  (symbol? exp))
  
;;if语句
(defn if? [exp]  (tagged-list? exp 'if)) 
(defn test-exp [exp] (cadr exp))
(defn then-exp [exp] (caddr exp))
(defn else-exp [exp] (cadddr exp))

(declare analyze-self-evaluting analyze-variable)

(defn analyze
  [exp]
  (cond 
    (self-evaluting? exp) (analyze-self-evaluting exp)
    (variable? exp) (analyze-variable exp)
    ))

(defn analyze-self-evaluting [exp]
  (fn [env] exp))
(defn analyze-variable [exp]
  (fn [env] (lookup-variable-value exp env)))

(defn scheme-eval [exp env]
  ((analyze exp) env))


;;解释器
(def input-prompt ";;; M-Eval input:")
(def out-prompt ";;; M-Eval value:")
(defn prompt-for-input [s]
  (newline)
  (newline)
  (print s)
  (newline))
(defn announce-output [s]
  (newline)
  (print s)
  (newline))

(defn drive-loop []
  (prompt-for-input input-prompt)
  (let [input (read)]
    (let [output (scheme-eval input *the-global-environment*)]
      (println "output " output)
      (announce-output out-prompt)
      (print output)))
  (recur))

