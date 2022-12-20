#lang eopl
;; Proyecto final
;;Integrantes          
;;Alejandro Lasso 2040393-3743
;;Alejandro Rodriguez 2042954-3743
;;Diana Cadena 2041260-3743
;;Marlon Anacona 2023777-3743
;;Robert Gil 2022985-3743
;;Juan Pablo Pantoja 2040104-3743

;Definición BNS (Backus-Naur form)
;
;  <program>       ::= <expresion>
;                      <a-program (exp)>
;  <expresion>    ::= <number> (numero)
;                  ::= <lit-exp (datum)>
;                  ::= <identificador>
;                      <var-exp (id)>
;                  ::= <valor-true> true
;                  ::= <valor-false> false
;                  ::= <empty-exp> null
;                  ::= <primitive> ({<expresion>}*(,))
;                      <primapp-exp (prim rands)>
;                  ::= <caracter>
;                      <caracter-exp>
;                  ::= <cadena>
;                     <cadena-exp>
;                  ::= begin {<expresion>}+(;) end
;                   ::= if (<expresion>) "{" <expresion>"}" else "{"<expresion>"}"
;                      <if-exp (exp1 exp2 exp23)> 
;                  ::= while (<expresion>) "{" <expresion>"}"
;                      <expresion> done
;                   ::= for (<identificador> = <expresion> <to> <expresion>) "{" <expresion>"}"
;                      <expresion> { <program>} fin
;                  ::= let {identificador = <expresion>}* in <expresion>
;                      <let-exp (ids rands body)>
;                  ::= rec  {identificador ({identificador}(,)) = <expresion>} in <expresion>
;                      <rec-exp proc-names idss bodies bodyrec>
;                  ::= proc({<identificador>}*(,)) <expresion>
;                      <proc-exp (ids body)>
;                  ::= (bas-8) bas8 <expresion> <bas8-exp> <expresion>;
;                  ::= (bas-16) bas16 <expresion> <bas16-exp> <expresion>;
;                  ::= (bas-32) bas32 <expresion> <bas32-exp> <expresion>;
;                  ::= (<expresion> {<expresion>}*)
;                       <app-exp proc rands>
;                  := (hex-number) (hex number {, number}*)
;                  := (oct-number) (oct number {, number}*)
;
; <lista>          ::= [{<expresion>} * (;)]
; <vector>         ::= vector[{<expresion>} * (;) ]
; <registro>       ::= { {<identificador> = <expresion> } + (;) }
; <expr-bool>      ::= <pred-prim>(<expresion> , <expresion>)
;                      <oper-bin-bool>(<expr-bool >, <expr-bool>)
;                  ::= <bool>
;                  ::= <oper-un-bool>(<expr-bool>)
;
; <primitive>      ::= + | - | * | add1 | sub1
;
; <pred-prim>      ::= < | <= | > | >= | == | != | && | || | <>
;
; <oper-bin-bool>  ::=  and | or 
; <oper-un-bool>   ::= (not-bool) not


;  <primitive-8>   ::= (suma8) +x8
;                  ::= (resta8) -x8
;                  ::= (multip8) *x8
;                  ::= (add16) ++x8
;                  ::= (rest16) --x8
;                  

;  <primitive-16>  ::= (suma16) +x16
;                  ::= (resta16) -x16
;                  ::= (multip16) *x16
;                  ::= (add16) ++x6
;                  ::= (rest16) --x6

;  <primitive-32>  ::= (suma32) +x32
;                  ::= (resta32) -x32
;                  ::= (multip132) *x32
;                  ::= (add32) ++x32
;                  ::= (rest32) --x32

;<expresion> ::= FNC <numero> ( clausula-or )+ (”and”)
;<clausula-or> ::= ( <numero> )+ (”or”)

;###########################################################

;Especificación Léxica

(define especificacion-lexica
'((espacio-en-blanco (whitespace) skip)
  (comentario ("/*" (arbno (not #\newline))) skip)
  (identificador(letter (arbno (or letter digit))) symbol)
  (null ("null") string)
  (numero (digit (arbno digit)) number) 
  (numero ("-" digit (arbno digit)) number)
  (float (digit (arbno digit)"."digit (arbno digit)) number)
  (float ("-" digit (arbno digit)"."digit (arbno digit)) number)
  (caracter ("'"letter"'") symbol)
  (cadena ("$"(or letter whitespace digit) (arbno (or whitespace letter digit))) string)
  )
)

;****************************************************************************************************************************************

;Especificación Sintáctica (gramática)

(define gramatica

  '(
    (program (expresion) a-programa)
    
    ;Definiciones:

    ;var 
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion) var-exp)

    ;const 
    (expresion ("const" (separated-list identificador "=" expresion ",") "in" expresion) const-exp)
    
    ;Datos

    ;identificador
    (expresion (identificador) ide-exp)

    ;numero (diferente entre enteros y floats)
    (expresion (num) num-exp)
    (num (numero) entero->numero)
    (num (float)  float->numero)

    ;expr-bool: 
    (expresion (expr-bool) expr-bool-exp)
    
    ;null-exp
    (expresion (null) null-exp)

    ;Constructores de Datos Predefinidos

    ;primitiva: forma de escribir una primitiva.
    (expresion ("[" primitiva (separated-list expresion ",") "]") primitive-exp)

    ;lista 
    (expresion ("lista (" (separated-list expresion ",") ")") list-exp)
    
    ;vector: 
    (expresion ("vector" "{"(separated-list expresion ",") "}") vector-exp)
    
    ;registro: 
    (expresion ("registro" "(" (separated-list identificador "->" expresion ";") ")") registro-exp)

    ;expresiones booleanas
    (expr-bool (pred-prim "(" expresion "," expresion ")") pred-prim-bool)
    (expr-bool (oper-bin-bool "(" expresion "," expresion")") oper-bin)
    (expr-bool (oper-un-bool"(" expresion")") oper-un)
    (expr-bool (boolean) bool-expr-bool)

    ;Valores de base bool
    (boolean ("true") true->boolean)
    (boolean ("false") false->boolean)

    ;primitivas booleanas
    (pred-prim (">") mayor-bool)
    (pred-prim (">=") mayor-igual-bool)
    (pred-prim ("<") menor-bool)
    (pred-prim ("<=") menor-igual-bool)
    (pred-prim ("==") igual-bool)
    (pred-prim ("!=") diferente-bool)

  
  
    
    ;Estructuras de Control

    ;begin
    (expresion ("begin" expresion ";" (separated-list expresion ";")"end") begin-exp)

    ;if
    (expresion ("if" "(" expresion")" "{" expresion "}" "else" "{" expresion "}") if-exp)

    ;while
    (expresion ("while" "("expresion")" "do" "{"expresion"}" ) while-exp)

    ;for
    (expresion ("for" "{" identificador "=" expresion ";" to expresion "}" "do" expresion) for-exp)
    (to ("to") to-for)
    (to ("downto") down-for)
    
    ;Asignación de Variables
    
    ;set
    (expresion ("set" identificador "=" expresion) set-exp)

    ;Procedimientos
    
    ;proc
    (expresion ("proc" "("(separated-list identificador ",") ")" "{" expresion "}") procedure-exp)
    
    ;invocar
    (expresion ("invocar" expresion "(" (separated-list expresion ",") ")") procedure-call-exp)
    
    ;rec
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion)  recursive-exp)
 
    ;Instancias B-SAT
    (expresion ("FNC" numero "(" (separated-list clausula-or "and") ")") instancia-sat-exp)
    (clausula-or ("("  (separated-list numero "or") ")") clausula-or-exp)
    (expresion ("*"identificador".solve()") solve-instancia-sat-exp)
 
    
    ;Primitivas aritméticas para enteros

    (primitiva ("+") primitiva-sum)
    (primitiva ("-") primitiva-rest)
    (primitiva ("*") primitiva-mult)
    (primitiva ("%") primitiva-mod)
    (primitiva ("/") primitiva-div)
    (primitiva ("add1") incr-prim)    
    (primitiva ("sub1") decr-prim)
   
    ;Primitivas sobre cadenas
    
    ;longitud
    (primitiva ("longitud") prim-longitud)

    ;concatenar
    (primitiva ("concatenar") prim-concatenar)

    ;primitivas sobre listas
    (primitiva ("vacio?") lst-vacio?)
    (primitiva ("vacio") lst-vacio)
    (primitiva ("lista?") lst-lista?)
    (primitiva ("crear-lista") lst-create)
    (primitiva ("cabeza") lst-cabeza)
    (primitiva ("cola") lst-cola)
    (primitiva ("append") lst-append)
    
    ;primitivas sobre vectores
    (primitiva ("vector?") vec-vector?)
    (primitiva ("ref-vector") vec-ref-vector)
    (primitiva ("set-vector") vec-set-vector)
    (primitiva ("crear-vector") vec-crear-vector)
    
  )
)
    
;******************************************Construcciones Automáticas****************************************

(sllgen:make-define-datatypes especificacion-lexica gramatica)
(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes especificacion-lexica gramatica)
  )
)

;Scan&parse

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica gramatica)
)
;Interpretador

(define interpreter
  (sllgen:make-rep-loop "---SMJ>" (lambda (pgm) (eval-program  pgm)) (sllgen:make-stream-parser especificacion-lexica gramatica))
)

;**********************************Interpretador********************************

;;Definición (Eval-Program)
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-programa (body)
        (eval-expression body (init-env))
      )
    )
  )
)

;;Ambiente Inicial
(define init-env
  (lambda ()
    (extend-env '() '() 
     (empty-env)
    )
  )
)

; Definición Eval-Expression

(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      ;Datos
      (ide-exp (id) (apply-env env id))
      (null-exp (null) 'null)
      (num-exp (numb) (implementacion-exp-numeros numb))
      (var-exp (vars vals body) (implementacion-exp-var vars vals body env))                                                     
      (const-exp (vars vals body) (implementacion-exp-cons vars vals body env))        
      (primitive-exp (prim list-expres) (implementacion-exp-primitivas prim list-expres env))  

      ;Constructores de Datos Predefinidos
      (list-exp (expr-list) (implementacion-exp-listas expr-list env))                                                 
      (vector-exp (expr-vec) (implementacion-exp-vectores expr-vec env))                                             
      (registro-exp (ids exps) (implementacion-exp-registros ids exps env))
      (expr-bool-exp (expres-bol) (implementacion-exp-booleanas expres-bol env))

      
      ;Estructuras de Control
      (begin-exp (expr exp-lists) (implementacion-exp-begin expr exp-lists env))
      (if-exp (bool-exp true-expr false-expr) (implementacion-exp-if bool-exp true-expr false-expr env))  
      (while-exp (bool-exp body) (implementacion-exp-while bool-exp body env))
      (for-exp (id init-value goto final-value body) (implementacion-exp-for id init-value goto final-value body env))    
                  
      
      ;Procedimientos
      (procedure-exp (ids body) (implementacion-exp-procedure         ids body env))
      (procedure-call-exp (expr args) (implementacion-exp-call-procedure        expr args env))
      (recursive-exp (proc-names idss bodies letrec-body) (implementacion-exp-recursivo proc-names idss bodies letrec-body env))


      ;Asignación de Variables
      (set-exp (id expr) (implementacion-exp-set id expr env))

      ;Instancias SAT                                              
      (instancia-sat-exp (first-int clauses)(implementacion-exp-sat first-int clauses ) )                                       
      (solve-instancia-sat-exp (id) (implementacion-exp-resolver-sat id env))   
      )
     )
   )

;********Paso por valor y por referencia**********
; Registra una referencia donde pos es la posicion de la referencia en el vector.

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)
         (mutable symbol?)
        )
  )

; Retorna el valor de la referencia del vector.

(define de-ref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vals mut) 
        (if (target? (vector-ref vals pos))
           (cases target (vector-ref vals pos)
              (indirect-target (refi) (primitive-deref refi))
           )
          (primitive-deref ref)
        )
      )
    )       
  )
)

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec mut)
             (vector-ref vec pos)
      )
    )
  )
)


;**Cambia el valor de la referencia por el valor dado**

(define setref!
  (lambda (ref val)
    (if (target? ref)
        (cases target ref
          (indirect-target (refi) (primitive-setref! refi val))
        )
        (primitive-setref! ref val)
    )
  )
)

(define primitive-setref!
     (lambda (ref val)
       (cases reference ref
         (a-ref (pos vec mut) (vector-set! vec pos val))
       )
     )
    )

;**Definición de Targets**

(define-datatype target target?
  (indirect-target (ref  ref-to-direct-target? ))

)

(define aplicar-indirect-target
  (lambda (val)
    (cases target val (indirect-target (ref)  (primitive-deref ref)))
  )
)

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec mut)
                  (if  (not (reference?  (vector-ref vec pos) ) )  #t #f )
                  )
           )
         )
    )
  )
  
  ; Definición de una variable cuando muta y cuando no.

(define-datatype variable variable?
  (mutable (id symbol?))
  (inmutable (id symbol?))
)
; Definición de una función para encontrar el simbolo en una lista de variables.

(define encontrar-sim-var
  (lambda (sym vars)
    (busqueda-symb-in-vars sym vars 0)
  )
)
; Definición de función auxiliar de busqueda simbolo en una lista de variables.

(define busqueda-symb-in-vars
  (lambda (sym vars pos)
    (cond
      ( (null? vars) #f)
      ( (equal? sym (retornar-simbolo (car vars))) pos )
      ( else (busqueda-symb-in-vars sym (cdr vars) (+ pos 1)) )
    )
  )
)
; Definición de función para encontrar en el ambiente la variable que tenga el mismo simbolo y retornarla.

(define var-es-mutable?
  (lambda (sym env)
      (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No se encontró ~s" sym))
      (extended-env-record (vars vals env)
                           (let ((pos (encontrar-sim-var sym vars)))
                             (if (number? pos)
                                 (cases variable (list-ref vars pos)
                                   (mutable (id) #t)
                                   (inmutable (id) #f)
                                 )
                                 (var-es-mutable? sym env)
                             )
        )
      )
  )
 )
)
; Definición que retorna cuando una variable es mutable o no.

(define encontrar-valor-mutable
 (lambda (sym vars)
   (busqueda-mut-in-vars sym vars 0)
 )
)

; Definición de función auxiliar que actúa como búsqueda en la lista de variables.

(define busqueda-mut-in-vars
  (lambda (sym vars pos)
    (cond
      ( (null? vars) #f)
      ( (equal? sym (retornar-simbolo (car vars))) (cases variable (car vars) (mutable (id) 'M) (inmutable (id) 'I) )  )
      ( else (busqueda-mut-in-vars sym (cdr vars) (+ pos 1)) )
    )
  )
)

; Definición que retorna el símbolo de la variable.

(define retornar-simbolo
  (lambda (var)
    (cases variable var
      (mutable (id) id)
      (inmutable (id) id)
    )
  )
)

;********************* Ambientes****************
;Definición del tipo de ambiente.

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (vars (list-of variable?))
                       (vec vector?)
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;Definición ambiente vacío.

(define empty-env  
  (lambda ()
    (empty-env-record)))       

;Extensión del ambiente.

(define extend-env
  (lambda (vars vals env)
    (extended-env-record vars (list->vector vals) env)))

;Procedimiento que busca un simbolo en un ambiente.

(define apply-env
  (lambda (env sym)
    (de-ref (apply-env-ref env sym))
  )
)

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "Simbolo desconocido ~s" sym))
      (extended-env-record (vars vals env)
                           (let ((pos (encontrar-sim-var sym vars)) (mut (encontrar-valor-mutable sym vars)) )
                             (if (and (number? pos) (symbol? mut) )
                                 (a-ref pos vals mut)
                                 (apply-env-ref env sym)
                                 )
                             )
                           )
      )
    )
  )
     


; Definición de true-valor como un valor de verdad.

(define true-value 'true)

; Definición de false-valor como un valor de falsedad.

(define false-value 'false)

;Definición que pregunta cuando algo es verdadero.

(define isTrue?
  (lambda (x)
    (equal? x true-value)
  )
)

(define-datatype sat sat?
  (instancia-sat (n integer?) (lista list?))
)
; Definición de datatype que define el tipo de dato árbol binario SAT.

(define-datatype arbol-sat arbol-sat?
  (nodo (value number?) (arbol-izquierda arbol-sat?) (arbol-derecha arbol-sat?))
  (arbol-vacio)
)

;***********************Desarrollo de Implementaciones*****************************************

; Implementación para evaluar expresiones númericas.

(define implementacion-exp-numeros
  (lambda (numb)
    (cases num numb
     (entero->numero (numb) numb)
     (float->numero (numb) numb)
    )
  )
)

; Implementación para evaluar expresiones booleanas.

(define implementacion-exp-booleanas
  (lambda (expr env)
    (cases  expr-bool expr
      (pred-prim-bool (pred first-expr second-expr)
                      (cases pred-prim pred
                        (menor-bool() (if (< (eval-expression first-expr env) (eval-expression second-expr env)) true-value false-value))
                        (mayor-bool() (if (> (eval-expression first-expr env) (eval-expression second-expr env)) true-value false-value))
                        (mayor-igual-bool() (if (>= (eval-expression first-expr env) (eval-expression second-expr env)) true-value false-value))
                        (menor-igual-bool() (if (<= (eval-expression first-expr env) (eval-expression second-expr env)) true-value false-value))
                        (igual-bool() (if (equal? (eval-expression first-expr env) (eval-expression second-expr env)) true-value false-value))
                        (diferente-bool() (if (not (equal? (eval-expression first-expr env) (eval-expression second-expr env))) true-value false-value))
                        )
                      )
      (oper-bin (pred first-expr second-expr)
                (cases oper-bin-bool pred
                  (and-boolean-primitive() (if (and (isTrue? (eval-expression first-expr env)) (isTrue? (eval-expression second-expr env))) true-value false-value))
                  (or-boolean-primitive() (if (or (isTrue? (eval-expression first-expr env)) (isTrue? (eval-expression second-expr env))) true-value false-value)) 
                  )
                )
      (oper-un (unary-prim  bool-exp)
               (cases oper-un-bool unary-prim
                 (not-boolean-primitive() (if (isTrue? (eval-expression bool-exp env)) false-value true-value))
                 )
               )
      (bool-expr-bool (bool)
                      (cases  boolean bool 
                        (true->boolean() true-value)
                        (false->boolean() false-value)
                        )
                      )
      )
    )
    
    )
    
    
    ; Implementación para evaluar procedimientos de tipo procval.

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)
   )
  )

; Función que evalua el cuerpo de un procedimientos en el ambiente extendido.

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env  (definir-mutabilidad ids args) args env)
                                )))))

; Función que define la mutabilidad de los argumentos envíados.



; Implementación de tipo procedure.

(define implementacion-exp-procedure        
  (lambda (ids body env)
    (closure ids body env)
  )
)
; Implementación de tipo call-procedure.

(define implementacion-exp-call-procedure       
  (lambda (expr args env)
    (let (
        (proc (eval-expression expr env))
          (argumentos  (implementacion-exp-listas args env))
         )  
         (if (procval? proc)
                     (apply-procedure proc argumentos)
                     (eopl:error 'eval-expression
                                 "No se puede aplicar el procedimiento para ~s" proc))
      )
  )
)

; Implementación de cadenas.

(define implementacion-exp-cadenas     
  (lambda (str)
    (substring str 1 (- (string-length str) 1))
  )
)

; Implementación de caracteres.

(define implementacion-exp-caracteres
  (lambda (str)
    (string->symbol (substring str 1 (- (string-length str) 1)))
  )
)

    

; Gramática
; <bignum> ::= (empty) | (number <bignum>)

; Definición de función zero que no recibe ningun argumento y retorna una lista vacia.

(define zero (lambda () empty ) )

; Definición de función is-zero? que recibe un numero cualquiera y determina si es igual a 0.

(define is-zero? (lambda (n) (null? n)))

; Definición de función successor que recibe como un argumento un numero entero no negativo y retorna el sucesor de ese numero.

(define successor
  (lambda (n)
    (cond
      [(is-zero? n) (cons 1 empty)]
      [(< (car n) 15) (cons (+ (car n) 1) (cdr n))]
      [else (cons 0 (successor (cdr n)))]
    )               
  )
)

; Definición de función predecessor que recibe como argumento un numero entero no negativo y retorna el predecesor de ese numero
; el predecesor de zero no esta definido.

(define predecessor
  (lambda (n)
    (cond
    [(is-zero? n) (eopl:error "no hay predecesor de cero" )]
    [(is-zero? (cdr n))
     (if (equal? (car n) 1) empty (cons (- (car n) 1) empty) )
    ]
    [(> (car n) 0) (cons (- (car n) 1) (cdr n))]
    [else (cons 15 (predecessor (cdr n)))]
   )
  )
)

; Implementación de listas.

(define implementacion-exp-listas
 (lambda (exprs env)
  (cond
   ((null? exprs) empty)
   (else
      (cons (eval-expression (car exprs) env) (implementacion-exp-listas (cdr exprs) env))
   )
  )
 )
)


; Implementación de vectores.

(define implementacion-exp-vectores
  (lambda (expr-vec env)
    (let ([v (make-vector (length expr-vec))] [i 0])
      (begin
        (for-each (lambda (arg) (begin (vector-set! v i (eval-expression (list-ref expr-vec i) env)) (set! i (+ i 1)))) expr-vec )
       v
      )
    )
  )
)


(define implementacion-exp-set
  (lambda (id expr env)
     (begin
       (if  (var-es-mutable? id env)
            (cases reference (apply-env-ref env id)
              (a-ref (pos vals mut)  
                  (if (target? (vector-ref vals pos))
                    (cases target (vector-ref vals pos) 
                      (indirect-target (ref)  (setref! ref (eval-expression expr env)))
                    )
                  (setref! (apply-env-ref env id) (eval-expression expr env))
                )
              )
            )      
            (eopl:error 'set-exp "No se puede modificar la constante ~s" id)
       )
       1
     )
  )
)



; Implementación de primitivas.

(define implementacion-exp-primitivas
  (lambda (prim list-expres env)
    (let ([exprs  (implementacion-exp-listas list-expres env)] )
    (cases primitiva prim

      ;Primitivas +,-,*,/,%,add1,sub1

      (primitiva-sum () (if (and (list? (car exprs)) (list? (cadr exprs)))(suma-base (car exprs) (cadr exprs))(+ (car exprs) (cadr exprs)))) 
      (primitiva-rest () (if (and (list? (car exprs)) (list? (cadr exprs))) (resta-base (car exprs) (cadr exprs)) (-   (car exprs) (cadr exprs)))) 
      (primitiva-mult () (if (and (list? (car exprs)) (list? (cadr exprs))) (multiplicacion-base (car exprs) (cadr exprs)) (* (car exprs) (cadr exprs))))   
      (primitiva-div () (/ (car exprs) (cadr exprs))) 
      (primitiva-mod () (modulo (car exprs) (cadr exprs)))
      (incr-prim () (if (list? (car exprs)) (successor (car exprs)) (+ (car exprs) 1)))              
      (decr-prim ()(if (list? (car exprs)) (predecessor (car exprs)) (- (car exprs) 1)))
      )
     )
    )
  )

     

; Implementación de begin.

(define implementacion-exp-begin
  (lambda (primera-exp lista-de-expresiones env)
     (if (null? lista-de-expresiones) (eval-expression primera-exp env)
         (begin (eval-expression primera-exp env) (implementacion-exp-begin (car lista-de-expresiones) (cdr lista-de-expresiones) env)))
  )
)

; Implementación de while.

(define implementacion-exp-while
        (lambda (bool-exp body env)
          (if (isTrue? (eval-expression bool-exp env)) (begin (eval-expression body env) (implementacion-exp-while bool-exp body env)) 'ok)
        )
)

; Implementación del if.

(define implementacion-exp-if
  (lambda (bool-exp true-expr false-expr env)
    (if (isTrue? (eval-expression bool-exp env)) (eval-expression  true-expr env) (eval-expression false-expr env))
  )
)

; Implementación ciclo for.

(define implementacion-exp-for
  (lambda (id init-value goto final-value body env)
    (for-recursivo id final-value body goto (extend-env (list (mutable id)) (list (eval-expression init-value env)) env))
  )
)


; Implementación ciclo for - recursivo.

(define for-recursivo
  (lambda (id final-value body goto env)
    (cases to goto
      (to-for () (if (< (apply-env env id ) (eval-expression final-value env)) (begin (eval-expression body env) (for-recursivo id final-value body goto env)) 1)   )
      (down-for () (if (> (apply-env env id ) (eval-expression final-value env)) (begin (eval-expression body env) (for-recursivo id final-value body goto env)) 1) )
    )   
  )
)

; Implementación expresión recursiva

(define implementacion-exp-recursivo
  (lambda ( proc-names idss bodies letrec-body env)
    (eval-expression letrec-body (extend-env-recursively proc-names idss bodies env))
  )
)


; Implementación auxiliar crea un ambiente extendido para procedimientos recursivos.

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record (map (lambda (id) (mutable id))proc-names) vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env))
            )
            (iota len) idss bodies
          )
          env
      )
     )
   )
  ))

; Implementación de instancias SAT

(define implementacion-exp-sat
  (lambda (first-int clauses )
    (instancia-sat first-int (implementacion-exp-or-sat clauses))
  )
)

(interpreter)


