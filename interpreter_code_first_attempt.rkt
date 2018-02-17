
;A first attempt at writing some of the interpreter functions
;Leo Mishlove

;#lang racket
(require "simpleParser.scm")

;to parse: type program code into a file, call (parser "filename"). parser returns parse tree in list format

;the main function that will encapsulate the parsing and evaluation
(define interpret
  (lambda (filename)
    (evaluate (parser filename))))

;the main function that will encapsulate the evaluation
(define evaluate
  (lambda (parsetree)
     ;placeholder code, not yet sure what will go here
     ;'(under construction)
    ;"Under construction"
    (error "Under construction")
    )
  )



;M_VALUE FUNCTIONS

;checks if something is a sub-expression
(define sub_expr?
  (lambda (s)
    (list? s)))

;binds numbers to their meanings
(define m_value_number
  (lambda (num)
    num))

;binds boolean values true and false to their meanings
(define m_value_boolean
  (lambda (boolean)
    (cond
      ((eq? boolean 'false) #f)
      ((eq? boolean 'true) #t)
      (else (error "Invalid boolean value")))))

;binds <value>s to their meanings
(define m_value_val
  (lambda (val)
    (if (number? val)
        (m_value_number val)
        (m_value_boolean val))))

;gets the value of a variable binding
(define m_value_var
  (lambda (var state)
    (if (eq? (lookup var state) 'not_found)
        (error "Variable not found")
        (m_value_val (lookup var state)))))

;finds the value of an expression, including variables and integer/boolean values
;<expr> -> <op><expr><expr>|<var>|<value>
;it checks to see which m_value expression is applicable (operator expression, variable, integer, boolean) and passes it off to that fcn
(define m_value_expr
  (lambda (expr state)
    (cond
      ((list? expr) (m_value_opr_expr expr state))
      ((number? expr) (m_value_number expr))
      ((booleanval? expr) (m_value_boolean expr))
      (else (m_value_var expr state))))) ;assumes that if expr not a list, number, or boolean, then it's  a variable

;finds the value of an expression with an operator
(define m_value_opr_expr
  (lambda (expr state)
    (cond
      ;arithmetic operators
      ((eq? (opr expr) '+) (+ (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state)))
      ((eq? (opr expr) '*) (* (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state)))
      ((eq? (opr expr) '/) (quotient (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state)))
      ((eq? (opr expr) '%) (remainder (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state)))
      ((and (eq? (opr expr) '-) (null? (cddr expr))) (- 0 (m_value_expr (op1 expr) state))) ;unary -: interprets it as 0 - 1. note: this will make unary - work on variables, e.g. (- x) evaluates to 0 - x.
      ((eq? (opr expr) '-) (- (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state)))
      ;still more operators to implement
      ;boolean operators
      ((eq? (opr expr) '&&) (and (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state))) ;assuming op1 and op2 are booleans - not typechecked
      ((eq? (opr expr) '||) (or (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state)))
      ((and (eq? (opr expr) '!) (null? (cddr expr))) (not (m_value_expr (op1 expr) state))) ;! is unary -- if you have ! x y or similar, cause an error.
      ;comparison operators
      ((eq? (opr expr) '==) (eq? (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state)))
      ((eq? (opr expr) '!=) (not (eq? (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state))))
      ((eq? (opr expr) '<) (< (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state)))
      ((eq? (opr expr) '>) (> (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state)))
      ((eq? (opr expr) '<=) (<= (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state)))
      ((eq? (opr expr) '>=) (>= (m_value_expr (op1 expr) state) (m_value_expr (op2 expr) state)))
      (else (error "Operator not recognized")))))
;^definitely not tail recursive or continuation-passing.


;M_STATE FUNCTIONS

;m_state_declare - the state after declaring a variable. takes a declaration statement decl and the current state state and gives the new value of the state.
(define m_state_declare
  (lambda (decl state)
    (cond
      ((eq? (decl_kind decl) 'dec_only) (m_state_dec_only decl state))
      ((eq? (decl_kind decl) 'dec_init) (m_state_dec_init decl state))
      (else (error "Interpreter error"))))) ;it shouldn't be entering this fcn unless we know it's a variable declaration, and variable declarations only start with 'var or '=.

;gives the state after a 'dec_only declaration statement decl and the current state state
(define m_state_dec_only
  (lambda (decl state)
    (cond
      ((eq? (lookup (varname decl) state) 'not_found) (state-add-val (varname decl) 'error state)) 
      (else (error "Variable already declared"))))) ;don't want to allow a variable to be declared again.

;gives the state after a 'dec_init declaration statement decl and the current state state
(define m_state_dec_init
  (lambda (decl state)
    (cond
      ((eq? (lookup (varname decl) state) 'not_found) (state-add-val (varname decl) (m_value_expr (assign_expr decl) state) state))
      (else (error "Variable already declared")))))

;gives the state after an assignment statement. takes the assignment statement, assign and the current state, state
(define m_state_assign
  (lambda (assign state)
    (cond
      ((eq? (lookup (varname assign) state) 'not_found) (error "Variable not initialized"))
      (else (state-add-val (varname assign) (m_value_expr (assign_expr assign) state) (state-remove (varname assign) state))))))
      



;HELPER FUNCTIONS

;opr - finds the operator of an expression
(define opr
  (lambda (expr)
    (car expr)))

;op1 - finds the first operand of an expression
(define op1
  (lambda (expr)
    (cadr expr)))

;op2 - finds the second operand of an expression
(define op2
  (lambda (expr)
    (caddr expr)))
     
;varname - gets the variable name of the left side of a declaration or assignment statement. works for both 'dec_only and 'dec_init declarations, as well as assignment statements.
(define varname
  (lambda (stmt)
    (cadr stmt)))

;takes a declaration or assignment statement stmt; gets the expression whose value is to be assigned to the var on the left hand side. works only for assignment statements and 'dec_init statements (bc in 'dec_only, cddr is null).
(define assign_expr
  (lambda (stmt)
    (caddr stmt))) ;cdr is (name (expr)) and cddr is ((expr)) so caddr is (expr). 


;lookup variable in the state and return its 'value'
;if want the value of its value, need (m_value (lookup var state))
;Currently built for a state of ((var1 var2 var3 ...) (value1 value2 value3))
;As of 2/15/18 have tested all cases.
(define lookup
  (lambda (var state)
    (cond
      ((not (state-valid? state)) (error "State construction error"))
      ((null? (var_list state)) 'not_found);(error "Undeclared variable")) ;not sure how to specify which variable
      ((and (eq? var (first_var state)) (null? (value_list state))) (error "State construction error"))
      ((eq? var (first_var state)) (first_value state))
      (else (lookup var (state_tail state)))))) ;Note that lookup is tail recursive

;adds a variable (and its value) to the state - returns a state
(define state-add-val
  (lambda (var value state)
    (cond
      ((not (state-valid? state)) (error "State construction error"))
      (else (list (cons var (var_list state)) (cons value (value_list state)))))))
;adds a variable (no value yet) to the state - returns a state
;(define state-add
;  (lambda (var state)
;    (cond
;      ((not (state-valid? state)) (error "State construction error"))
;      (else (list (cons var (var_list state)) (value_list state))))))
;doing it this way will mess with the integrity of the list, b/c different length of each of the lists. e.g. adding y to '((x z) (1 2)) returns '((y x z) (1 2)) and then when you try to get the value of y, you get 1, and that's wrong.
;so instead, will initialize 'var declarations with 'error.

;removes a variable (and its value) from the state - returns a state
(define state-remove-bad
  (lambda (var state)
    (cond
      ((not (state-valid? state)) (error "State construction error"))
      ((null? (var_list state)) state)
      ((null? (value_list state)) (error "State construction error"))
      ((eq? var (first_var state)) (state_tail state))
      (else (list (cons (first_var state) (var_list (state-remove-bad var (state_tail state)))) (cons (first_value state) (value_list (state-remove-bad var (state_tail state)))))))))

;cps tail recursive version of state-remove-bad
;return = (lambda (v1 v2) (list v1 v2))
(define state-remove-cps
  (lambda (var state return)
    (cond
      ((not (state-valid? state)) (error "State construction error"))
      ((null? (var_list state)) (return '()()))
      ((null? (value_list state)) (error "State construction error"))
      ((eq? var (first_var state)) (return (var_list (state_tail state)) (value_list (state_tail state))));(state_tail state))
      (else (state-remove-cps var (state_tail state) (lambda (vars values) (return (cons (first_var state) vars) (cons (first_value state) values ))))))))

;cover for state-remove-cps
(define state-remove
  (lambda (var state)
    (state-remove-cps var state (lambda (v1 v2) (list v1 v2)))))



;var_list - gets the list of variable names in the state
(define var_list
  (lambda (state)
    (car state)))

;value_list - gets the list of value names in the state
(define value_list
  (lambda (state)
    (cadr state))) ;car of cdr of state is the second list in state.

;gets the first variable in the state
(define first_var
  (lambda (state)
    (car (var_list state))))

;gets the first value in the state
(define first_value
  (lambda (state)
    (car (value_list state))))

;gets the tail end of the state: the state minus the first variable and its value
;2/15/18 all cases tested
(define state_tail
  (lambda (state)
    (cond
      ((not (state-valid? state)) (error "State construction error"))
      ((null? (var_list state)) (error "State construction error"))
      ((null? (value_list state)) (error "State construction error"))
      (else (list (cdr (var_list state)) (cdr (value_list state)))))))


;returns #t or #f depending on whether state is valid.
;Note: does not catch things like '((x)()) or '(()(10))
;2/15/18 all existing cases tested.
(define state-valid?
  (lambda (state)
    (cond
      ((null? state) #f)
      ((null? (cdr state)) #f)
      ((and (list? (var_list state)) (list? (value_list state))) #t)
      (else #f))))

;checks if a value is 'true or 'false
(define booleanval?
  (lambda (val)
    (cond
      ((eq? val 'true) #t)
      ((eq? val 'false) #t)
      (else #f))))

;takes a declaration statement and tells you what kind of declaration it is: 'dec_only, which only declares, or 'dec_init, which declares and initializes.
(define decl_kind
  (lambda (decl)
    (cond
      ((null? (cddr decl)) 'dec_only)
      (else 'dec_init))))

  