;Taylor Woodcock
;Leo Mishlove
;EECS 345 -- Interpreter Part 1

;HOW TO USE:
; 1. Type program code into a plain text file. Save it in the same directory as this file (as well as simpleParser.scm and lex.scm)
; 2. Call (interpret "filename")
; 3. The program will return the appropriate value


(require "simpleParser.scm")

;to parse: type program code into a plain text file, call (parser "filename"). parser returns parse tree in list format

;the main function that will encapsulate the parsing and evaluation
(define interpret
  (lambda (filename)
    (evaluate (parser filename))))

; calls helper function eval-rec - call/cc used here to allow for immediate return
(define evaluate
  (lambda (parsetree)
    (call/cc
      (lambda (return)
        (eval-rec parsetree state_init return)))))

; short for recursive eval - calls m_state_eval for every element of the parsetree until it's empty
(define eval-rec
  (lambda (parsetree state return) 
    (cond
      ((null? parsetree) (error "Missing return statement"))
      (else (eval-rec (cdr parsetree) (m_state_eval (car parsetree) state return) return))))) ;hey, tail recursion! nice


;------------------------------------------------------------------------------------------
;M_STATE FUNCTIONS

;m_state_declare - the state after declaring a variable. takes a declaration statement decl and the current state state and gives the new value of the state.
(define m_state_declare
  (lambda (decl state)
    (cond
      ((eq? (decl_kind decl) 'dec_only) (m_state_dec_only decl state))
      ((eq? (decl_kind decl) 'dec_init) (m_state_dec_init decl state))
      (else (error "Interpreter error"))))) ;it shouldn't be entering this fcn unless we know it's a variable declaration, and variable declarations are only of those two kinds

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

; evaluates expressions. what more do you want from me
(define m_state_eval
  (lambda (expr state return)
    (cond
      ((null? expr) (error "Undefined expression"))
      ((eq? (opr expr) '=) (m_state_assign expr state))
      ((eq? (opr expr) 'var) (m_state_declare expr state))
      ((eq? (opr expr) 'while) (m_state_loop expr state return))
      ((eq? (opr expr) 'if) (m_state_cond expr state return))
      ((eq? (opr expr) 'return) (m_state_return (ret-expr expr) state return))
      (else (error "Invalid expression")))))

; evaluates while loops - this is also tail recursive, which is neat
(define m_state_loop
  (lambda (expr state return)
    (if (m_value_expr (condition expr) state) (m_state_loop expr (m_state_eval (loop-body expr) state return) return) state)))

; evaluates if/else statements - the m_state_eval can call m_state_cond, which allows for nested statements
(define m_state_cond
  (lambda (expr state return)
    (if (m_value_expr (condition expr) state) (m_state_eval (then-stmt expr) state return) (m_state_eval (else-stmt expr) state return))))

; returns the value of the expression given
(define m_state_return
  (lambda (expr state return)
    (cond
      ((eq? (m_value_expr expr state) #t) (return 'true))
      ((eq? (m_value_expr expr state) #f) (return 'false))
      (else (return (m_value_expr expr state))))))


;------------------------------------------------------------------------------------------
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
      ((boolean? boolean) boolean) ;if it's already #t or #f, it's good to go.
      (else (error "Invalid boolean value")))))

;binds <value>s to their meanings
(define m_value_val
  (lambda (val)
    (cond
      ((eq? val 'error) (error "Variable not assigned"))
      ((number? val) (m_value_number val))
      (else (m_value_boolean val)))))

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


;------------------------------------------------------------------------------------------
;HELPER FUNCTIONS

;gives the initial state structure for an empty state.
(define state_init
  (lambda ()
    '(()())))

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

;gets the expression to be returned from a return statement - functional equivalent of op1
(define ret-expr
  (lambda (ret-stmt)
    (cadr ret-stmt)))

;gets the loop body to be executed - functional equivalent of op2
(define loop-body
  (lambda (while-stmt)
    (caddr while-stmt)))

;given an if/else or while statement, gets the conditional expression to be evaluated - functional equivalent of op1
(define condition 
  (lambda (stmt)
    (cadr stmt)))
;gets the statement to be done if the conditional evaluates to #t - functional equivalent of op2
(define then-stmt
  (lambda (if-else)
    (caddr if-else)))
;gets the statement to be done if the conditional evaluates to #f
(define else-stmt
  (lambda (if-else)
    (cadddr if-else)))

;lookup variable in the state and return its 'value'
;if want the value of its value, need (m_value (lookup var state))
;Currently built for a state of ((var1 var2 var3 ...) (value1 value2 value3))
;As of 2/15/18 have tested all cases.
(define lookup
  (lambda (var state)
    (cond
      ((null? (var_list state)) 'not_found);(error "Undeclared variable")) ;not sure how to specify which variable
      ((and (eq? var (first_var state)) (null? (value_list state))) (error "State construction error"))
      ((eq? var (first_var state)) (first_value state))
      (else (lookup var (state_tail state)))))) ;Note that lookup is tail recursive

;adds a variable (and its value) to the state - returns a state
;above, we use 'error for the value if the variable is declared but not initialized
(define state-add-val
  (lambda (var value state)
    (list (cons var (var_list state)) (cons value (value_list state)))))

;removes a variable (and its value) from the state - returns a state
;return = (lambda (v1 v2) (list v1 v2))
(define state-remove-cps
  (lambda (var state return)
    (cond
      ((null? (var_list state)) (return '()()))
      ((null? (value_list state)) (error "State construction error"))
      ((eq? var (first_var state)) (return (var_list (state_tail state)) (value_list (state_tail state))))
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
      ((null? (var_list state)) (error "State construction error"))
      ((null? (value_list state)) (error "State construction error"))
      (else (list (cdr (var_list state)) (cdr (value_list state)))))))

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

  