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

;###########################################################

;Especificación Léxica

(define especificacion-lexica
'((espacio-en-blanco (whitespace) skip)
  (comentario ("/*" (arbno (not #\newline))) skip)
  (identificador (letter (arbno (or letter digit "."))) symbol)
  (null ("null") string)
  (numero (digit (arbno digit)) number) 
  (numero ("-" digit (arbno digit)) number)
  (float (digit (arbno digit)"."digit (arbno digit)) number)
  (float ("-" digit (arbno digit)"."digit (arbno digit)) number)
  (caracter ("'"letter"'") symbol)
  (cadena ("$"(or letter whitespace digit) (arbno (or whitespace letter digit))) string)
  )
)

;###########################################################

;Especificación Sintáctica (gramática)

(define gramatica

  '(
    (program ((arbno class-decl) expresion) a-program)
    
    ;Definiciones:

    ;var
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion) var-exp)

    ;const
    (expresion ("const" (separated-list identificador "=" expresion ",") "in" expresion) const-exp)
    
    ;Datos

    ;identificador
    (expresion (identificador) ide-exp)

    ;numero:(diferenciación entre enteros y floats)
    (expresion (num) num-exp)
    (num (numero) entero->numero)
    (num (float)  float->numero)


    ;caracter
    (expresion (caracter) caracter-exp)

    ;cadena 
    (expresion (cadena) cadena-exp)
    
     ;& referencia
    (expresion ("&" identificador) refe-exp)

    ;imp-exp
    (expresion ("print("expresion")") imp-exp)
    
    ;expr-bool
    (expresion (expr-bool) expr-bool-exp)
    
    ;null-exp
    (expresion (null) null-exp)

    ;Constructores de Datos Predefinidos

    ;primitiva: forma de escribir una primitiva.
    (expresion ("[" primitiva (separated-list expresion ",") "]") primitive-exp)

    ;lista
    (expresion ("lista (" (separated-list expresion ",") ")") list-exp)
    
    ;vector
    (expresion ("vector" "{"(separated-list expresion ",") "}") vector-exp)
    
    ;registro
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

    ;primitivas binarias booleanas
    (oper-bin-bool ("and") and-boolean-primitive)
    (oper-bin-bool ("or")  or-boolean-primitive)

    ;primitiva not booleana
    (oper-un-bool ("not") not-boolean-primitive)
    
    ;Definición de expresiones hexadecimales

    ;Base [8,16,32]
    
    (expresion ("x8(" (arbno expresion)")") hexadecimal-exp)
    (expresion ("x16(" (arbno expresion)")") hexadecimal-exp)
    (expresion ("x32(" (arbno expresion)")") hexadecimal-exp)
    
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
    
    ;;class-decl
     (class-decl ("class" identificador "extends" identificador (arbno "atr" identificador) (arbno method-decl)) a-class-decl)

    ;;method-decl
    (method-decl ("def" identificador "(" (separated-list identificador ",") ")" ":" expresion ) a-method-decl)

    ;;new-object
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-object-exp)
    
    ;;super-call
    (expresion ("super" identificador "(" (separated-list identificador ",") ")") super-call-exp)

    ;;method-app-exp
    (expresion ( "inv" expresion "->" identificador "("  (separated-list expresion ",") ")") method-app-exp)
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

   ;Primitivas [Listas, Vectores, Registros]

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
    
    ;primitivas sobre registros
    (primitiva ("registro?") reg-reg?)
    (primitiva ("ref-registro") reg-ref)
    (primitiva ("set-registro") reg-set)
    (primitiva ("crear-registro") reg-crear)
    
  )
)
    
;||||||||||||||||||||||||Construcciones Automáticas||||||||||||||||||||||||

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
  (sllgen:make-rep-loop "-MINIPY>" (lambda (pgm) (eval-program  pgm)) (sllgen:make-stream-parser especificacion-lexica gramatica))
)

;||||||||||||||||||||||||Interpretador||||||||||||||||||||||||
;;evalua un programa
;;Definición (Eval-Program)
(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls exp)
        (elaborate-class-decls! c-decls)
        (eval-expression exp (init-env))))))

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
      (var-exp (vars vals body) (let ((args (eval-variableLocal-exp-rands vals env)))
                 (eval-expression body (extend-env vars args env))))                                                     
      (const-exp (vars vals body) (let ((args (eval-variableLocal-exp-rands vals env)))

                           (if (updateValExp? body)
                               (eopl:error 'eval-expression
                                "No puedes actualizar los valores de constantes ~s" body)
                               (eval-expression body (extend-env vars args env)))
                           ))
      (cadena-exp (str) (implementacion-exp-cadenas      str))
      (caracter-exp (char) (implementacion-exp-caracteres char))
      (primitive-exp (prim list-expres) (implementacion-exp-primitivas prim list-expres env))  
      (refe-exp (ref)
      (cases reference (apply-env-ref env ref)
        (a-ref (pos vals) 
               (if (target? (vector-ref vals pos) )
                   (vector-ref vals pos)
                   (indirect-target (apply-env-ref env ref))))))

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
      (recursive-exp (proc-names idss bodies letrec-body) (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))

      ;Asignación de Variables
      (set-exp (id newVal)
               (setref!
                  (apply-env-ref env id)
                  (eval-expression newVal env)))
      

      ;Hexadecimales [Base 8,16,32]
      (hexadecimal-exp (hex) (eval-expresiones-hexadecimales hex env))

      ;Impresión de Variables                  
      (imp-exp (ex) (display  (eval-expression ex env)))

     ; POO
      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply 'init class-name obj args)
          obj
          ))

      ;; para las expresiones de invocación de métodos (send)
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expression obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))

      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply
            method-name (apply-env env '%super) obj args)))                                  
    )
  )
)
(define updateValExp?
  (lambda (body)
    (cases expresion body
      (procedure-exp (ids cuerpo)
          (updateValExp? cuerpo))
      
      (var-exp (ids values body)
                        (updateValExp? body))
      (const-exp (ids values body)
                        (updateValExp? body) )

      (set-exp (id newVal)
               #t)

      (begin-exp (exp exps)
                 (if (updateValExp? exp)
                     #t
                     (let loop (
                                 (exps exps)
                               )
                       (if (null? exps)
                           #f
                           (if (updateValExp? (car exps))
                               #t
                               (loop (cdr exps))
                           )
                       ) 
                     )
                 )
        )
      (recursive-exp (proc-names idss bodies letrec-body)
                  (updateValExp? letrec-body))      
      (else #f)
    )
  )
)
;||||||||||||||||||||||||[Paso por valor y paso por referencia]||||||||||||||||||||||||

; Registra una referencia donde pos es la posicion de la referencia en el vector.

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

; Retorna el valor de la referencia del vector.

(define de-ref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vals) 
        (if (target? (vector-ref vals pos))
           (cases target (vector-ref vals pos)
             (direct-target (expval) expval)
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
      (a-ref (pos vec)
             (vector-ref vec pos)
      )
    )
  )
)


;Cambia el valor de la referencia por el valor dado

(define setref!
  (lambda (ref expval)
    (let ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))
(define expval?
  (lambda (x)
    (or (number? x)  (string? x) (procval? x)(vector? x) (boolean? x) (list? x))))

; Definición de Targets

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref  ref-to-direct-target? ))

)



(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))
; Definición de una variable cuando muta y cuando no.



  ;||||||||||||||||||||||||[Definición de Ambientes]||||||||||||||||||||||||

;Definición del tipo de ambiente.

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (vars (list-of symbol?))
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
      (empty-env-record () (eopl:error 'apply-env-ref "No se encontro en el ambiente a ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))
;Funciones Auxiliares
(define eval-variableLocal-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-variableLocal-exp-rand x env))
         rands)))

(define eval-variableLocal-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env)) 
    ))
; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente
(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))
     

; Definición que retorna una lista de los números desde 0 hasta end

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

; Definición de tipos de datos nativos.

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

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

;||||||||||||||||||||||||[Desarrollo de Implementaciones]||||||||||||||||||||||||

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
               (eval-expression body (extend-env ids args env))))))




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

; Implementación de hexadecimales.

(define eval-expresiones-hexadecimales
  (lambda (expr env)
     (implementacion-exp-listas expr env)
  )
)
(define suma-base
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma-base (predecessor x) y)))))

(define resta-base
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta-base  x (predecessor y))))))

(define multiplicacion-base
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma-base (multiplicacion-base (predecessor x) y) y))
    ))
    

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

; Implementación de registros.

(define implementacion-exp-registros
  (lambda (ids exps env)
    (registro-base (map (lambda (id) (symbol->string id) ) ids)  (list->vector (implementacion-exp-listas exps env)) )
  )
)
(define values? 
  (lambda (any)
    #t
  )
)
(define-datatype registro registro?
  (registro-base (ids (list-of string?)) (vals vector? ))
  
)
(define pos-simbolo 
  (lambda (simbolo lista pos)
    (cond
      [(null? lista) -1]
      [(equal? simbolo (car lista)) pos]
      [else (pos-simbolo simbolo (cdr lista) (+ pos 1))]
    )
  )

)
(define set-registro 
  (lambda (id val lista vec)
    (begin
      (define pos (pos-simbolo id lista 0))
      (if (equal? -1 pos) (eopl:error "Tecla inválida") (vector-set! vec pos val))
    )
  
  )
)
(define get-registro
  (lambda (id  lista vec)
    (begin
      (define pos (pos-simbolo id lista 0))
      (if (equal? -1 pos) (eopl:error "Tecla inválida") (vector-ref vec pos))
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

      ;Primitivas para strings (concat-long).
      
      (prim-concatenar () (string-append (car exprs)(cadr exprs)))
      (prim-longitud () (string-length (car exprs)))

      ;Primitivas para listas.
      (lst-create () (cons (car exprs) (cadr exprs)) )
      (lst-append () (append (car exprs) (cadr exprs)) )
      (lst-vacio? () (if (equal? (car exprs) empty) true-value false-value))
      (lst-lista? () (if (list? (car exprs)) true-value false-value ))
      (lst-cabeza () (caar exprs))
      (lst-cola () (cdr(car exprs)))
      (lst-vacio ()empty)

      ;Primitivas para vectores.
      
      (vec-ref-vector () (vector-ref (car exprs) (cadr exprs)))
      (vec-set-vector() (begin (vector-set! (car exprs) (cadr exprs) (caddr exprs)) 'CambioExitoso))
      (vec-vector? () (if (vector? (car exprs)) true-value false-value))
      (vec-crear-vector () (make-vector (car exprs) 'CreaciónDeVectorExitoso))

      ;Primitivas para registros.
      
      (reg-reg? () (if (registro? (car exprs)) true-value false-value))
      (reg-set () (if (registro? (car exprs)) (cases registro (car exprs)(registro-base ( ids vals) (begin (set-registro (cadr exprs) (caddr exprs) ids vals) 1) ) )  (eopl:error "No es un registro")))
      (reg-ref () (if (registro? (car exprs)) (cases registro (car exprs)(registro-base (ids vals)  (get-registro (cadr exprs) ids vals ))) (eopl:error "No es un registro")))
      (reg-crear () (registro-base (car exprs) (list->vector (cadr exprs)))) 
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
          (if (isTrue? (eval-expression bool-exp env)) (begin (eval-expression body env) (implementacion-exp-while bool-exp body env)) '⠀)
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
    (letrec
                   [(iterador (eval-expression init-value env))
                    (parada (eval-expression final-value env))
                    (op (cases to goto
                          (to-for () +)
                          (down-for () -)
                          ))
                    (proc-for (closure (list id) body env))
                    (for (lambda (var)                           
                           (if (eqv? var parada)
                               (apply-procedure proc-for (list (direct-target var)))
                               (begin (apply-procedure proc-for (list (direct-target var))) (for (op var 1)))
                               )
                               )
                           )]
                 (for iterador)
                   )
  )
)






; Implementación auxiliar crea un ambiente extendido para procedimientos recursivos.

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))

; Implementación para evaluar la combinación que es correcta.

(define implementacion-evaluar-combinacion-correcta
  (lambda (combinaciones clausulas-or pos)
    (if (equal? pos (vector-length combinaciones))
        "Insatisfactible"
        (if (evaluar-combinacion (list->vector (vector-ref combinaciones pos)) clausulas-or )
            (vector-ref combinaciones pos)
            (implementacion-evaluar-combinacion-correcta combinaciones clausulas-or (+ pos 1))
        )
    )
  )
)
; Implementación evaluar-combinación que retorna true sí todas las clausulas-or en la lista son verdad al ser evaluadas.

(define evaluar-combinacion
  (lambda (combinacion clausulas-or)
    (if (null? clausulas-or)
        #t
        (and (evaluar-clausula combinacion (car clausulas-or)) (evaluar-combinacion combinacion (cdr clausulas-or)))
    )
  )
)
; Implementación evaluar-clausula que retorna true si al menos un valor dentro de la clausula or es verdad.

(define evaluar-clausula
  (lambda  (combinacion clausula)
    (if (null? clausula)
        #f
        (or (traducir-valor-de-verdad combinacion (car clausula)) (evaluar-clausula combinacion (cdr clausula)))
    )
  )
)

; Implementación traducir valor verdad que recibe un vector con numeros enteros distintos a 0 y retorna el valor de verdad.

(define traducir-valor-de-verdad
  (lambda (vec pos)
    (if (< pos 0)
        (not (obtener-valor-de-verdad vec (- (* pos -1) 1 )))
        (obtener-valor-de-verdad vec (- pos 1))
    )
  )
)
; Implementación obtener valor de verdad que retorna T sí el valor a que hace referencia en el vector es 1, de lo contrario es 0.

(define obtener-valor-de-verdad
  (lambda (vec pos)
    (begin
      (define val (vector-ref vec pos))
      (if (equal? val 1) #t #f)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;POO;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ambiente de declaracion de clases

(define the-class-env '())
;; Almacena las clases
(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

;; Busca las clases
(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Class not known ~s" name))
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;; Ambiente de métodos

;; Busca los metodos de una clase
(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))

;; Auxiliares para el manejo de declaraciones de clases y métodos

(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        (map (lambda (id) (string->symbol (string-append "self." (symbol->string id)))) field-ids)))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))

;;;;;;;;;;;;;;;;;; Selectores ;;;;;;;;;;;;;;;;;;

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))

(define part->super-name
  (lambda (part)
    (class-decl->super-name (part->class-decl part))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))

(define-datatype part part? 
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (eqv? class-name 'objeto)
      '()
      (let ((c-decl (lookup-class class-name)))
        (cons
          (make-first-part c-decl)
          (new-object (class-decl->super-name c-decl)))))))

(define make-first-part
  (lambda (c-decl)
    (a-part
      (class-decl->class-name c-decl)
      (make-vector (length (class-decl->field-ids c-decl))(direct-target 0)))))


;;;;;;;;;;;;;;;;;;;; Métodos ;;;;;;;;;;;;;;;;;; 

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'objeto)
      (if (eqv? m-name 'init)
          (eopl:error 'find-method-and-apply
        "~s method was not provided" m-name)
          (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name))
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
           (apply-method m-decl host-name self args)
          (find-method-and-apply m-name 
            (class-name->super-name host-name)
            self args))))))

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
      parts
      (view-object-as (cdr parts) class-name))))

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (eval-expression body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (build-field-env 
            (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
      (empty-env)
      (extend-env-refs
        (part->field-ids (car parts))
        (part->fields    (car parts))
        (build-field-env (cdr parts))))))

(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))


(interpreter)

;||||||||||||||||||||||||Scan&Parser||||||||||||||||||||||||



;Definiciones
;(scan&parse "var y=50, x=20 in [+ x,y]")
;(scan&parse "const x=4 in set x=1") //No se puede modificar la constante x.

;Datos
;(scan&parse "5")
;(scan&parse "-100")
;(scan&parse "5.5")
;(scan&parse "a")
;(scan&parse "$abc")
;(scan&parse "true")
;(scan&parse "false")
;(scan&parse "'w'")
;(scan&parse "null")
;(scan&parse "&STV") //Paso por referencia

;Constructores de Datos Predefinidos
;(scan&parse "lista (4,2)")
;(scan&parse "vector {4,2,a,b}")
;(scan&parse "registro (a->4; b->6; c->8)")
;(scan&parse "> (5,3)")
;(scan&parse ">= (10,5)")
;(scan&parse "< (5,3)")
;(scan&parse "<= (10,5)")
;(scan&parse "== (5,4)")
;(scan&parse "!= (5,5)")

;Estructuras de Control
;(scan&parse "begin set z = 1; set p = 9 end")
;(scan&parse "var y=5 in begin set y= [+ y,1]; print(y) end")
;(scan&parse "if( >(5,3)) {5} else {0} ")
;(scan&parse "if( >(5,3)) {true} else {false} ")
;(scan&parse "if (==(5,9)) {if (>=(10,20)){6} else {3}} else{4}")
;(scan&parse "var x = 5,y=0 in while (>(x,y)) do {begin set y= [+ y,1]; print(y) end}")
;(scan&parse "for {x=2; to 5} do $a")
;(scan&parse "for {x=2; to 100} do proc(a,b,c) {true}")

;Procedimientos
;(scan&parse "proc(a,b,c){true}")
;(scan&parse "invocar x (registro (a->1; b->2))")
;(scan&parse "rec f(x)= if(==(x,0)){1} else {[* x,invocar f([sub1 x])]} in invocar f(5)")

;Primitivas para Enteros
;(scan&parse "[+ 10,10]")
;(scan&parse "[- 5,5]")
;(scan&parse "[* 20,5]")
;(scan&parse "[% 100,10]")
;(scan&parse "[/ 1000,50]")
;(scan&parse "[add1 150]")
;(scan&parse "[sub1 200]")

;Primitivas para Flotantes
;(scan&parse "[+ 5.9,2.8]")
;(scan&parse "[- 1.1,1.0]")
;(scan&parse "[* 500.25,0]")
;(scan&parse "[/ 500.100,250.211]")
;(scan&parse "[add1 205.5]")
;(scan&parse "[sub1 500]")
;(scan&parse "[+ 5.9,2.8,[- 2,3]]")

;Primitivas para Hexadecimales [Base 8]
;(scan&parse "x8(2 1 4 0 7)")
;(scan&parse "x8([+ 5,5])")
;(scan&parse "x8([- 7, 5])")
;(scan&parse "x8([* 10,10])")
;(scan&parse "x8([/ 5,5])")
;(scan&parse "x8([add1 2])")
;(scan&parse "x8([sub1 1])")

;(scan&parse "x16(4 1 0 7 14)")
;(scan&parse "x16([+ 700,500])")
;(scan&parse "x16([- 1997,2022])")
;(scan&parse "x16([* 5,5])")
;(scan&parse "x16([/ 12,12])")
;(scan&parse "x16([add1 999])")
;(scan&parse "x16([sub1 1000])")

;(scan&parse "x32(0 23 12)")
;(scan&parse "x32([+ 1,2])")
;(scan&parse "x32([- 23,12])")
;(scan&parse "x32([* 23,5])")
;(scan&parse "x32([/ 0,12])")
;(scan&parse "x32([add1 12])")
;(scan&parse "x32([sub1 5])")

;Primitivas para Cadenas
;(scan&parse "[longitud $fundamentos]")
;(scan&parse "[concatenar $flp , $II ]")
;(scan&parse "[longitud [concatenar $hola , $mundo ]]")

;Primitivas para Listas
;(scan&parse "[vacio? lista (1,2,3)]")
;(scan&parse "[vacio? lista ()])"
;(scan&parse "[vacio lista ()]")
;(scan&parse "[vacio lista (1,2,3)]")
;(scan&parse "[lista? 1,2,3]")
;(scan&parse "[crear-lista 1,2,3]")
;(scan&parse "[crear-lista 1,$hola ]")
;(scan&parse "[cabeza lista (17,$a , 21)]")
;(scan&parse "[cola lista (17,$a , 21)]")
;(scan&parse "[append lista (17,$a , 21), lista (5,4)]")

;Primitivas para Vectores
;(scan&parse "vector{0,1,2,3,4,5}")
;(scan&parse "[vector? vector{1,2,3}]")
;(scan&parse "[vector? [crear-vector 1,2,3,4]]")
;(scan&parse "[crear-vector 1,2,3,4]")
;(scan&parse "[ref-vector vector{1,2,3,10,15},4]")
;(scan&parse "[set-vector vector{1,2,3,10,15},4,50]")

;Primitivas para Registros
;(scan&parse "[registro? registro(a->4; b->$hola)]")
;(scan&parse "registro(a->4; b->$prueba ; c->123)")
;(scan&parse "[ref-registro registro(a->$holamundo; b->$100)]")
;(scan&parse "[crear-registro registro(a->4; b->$hola), [set-registro registro(a->5; b->$mundo)]]")

(scan&parse "
class nodo extends objeto
  atr left
  atr right
  def init(left, right):
     begin
       set self.left=left;
       set self.right=right
     end
  def sumar(pred): [+ inv self.left->sumar(pred), inv self.right->sumar(pred)]
class hoja extends objeto
  atr valor
  def init(n): set self.valor= n
  def sumar(pred): if (invocar pred(self.valor)) {self.valor} else {0}
const arbol=new nodo(
                   new nodo(new hoja(1),new hoja(6)), 
                   new nodo(
                      new nodo(new hoja(3), new hoja(13)), 
                      new hoja(11)))
     in inv arbol->sumar(proc(n){>(n, 10)})")
(scan&parse "
rec
  fibo(n)= if (> (n, 1))
              {[+ invocar fibo([- n, 1]), invocar fibo([- n, 2])]}
             else {1}
  fibonacci(n)= if (==(n, 0))
                    {lista (1)}
                   else
                    {[append
                        invocar fibonacci([- n,1]),
                        lista (invocar fibo(n)) ]}
in invocar fibonacci(5)")
