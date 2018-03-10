;Taylor Woodcock
;Leo Mishlove
;EECS 345 -- Interpreter Part 2

;HOW TO USE:
; 1. Type program code into a plain text file. Save it in the same directory as this file (as well as simpleParser.scm and lex.scm)
; 2. Call (interpret "filename")
; 3. The program will return the appropriate value

;------------------------------------------------------------------------------------------
;RESOURCES

(require "simpleParser.scm") ;to parse: type program code into a plain text file, call (parser "filename"). parser returns parse tree in list format




;------------------------------------------------------------------------------------------
;PRIMARY FUNCTIONS

;1. interpret -- the main function that will encapsulate the parsing and evaluation
(define interpret
  (lambda (filename)
    (evaluate (parser filename))))

;2. evaluate -- calls helper function eval-rec - call/cc used here to allow for immediate return
(define evaluate
  (lambda (parsetree)
    (call/cc
      (lambda (return)
        (eval-rec parsetree (state_init) return)))))

;3. eval-rec -- short for recursive eval - calls m_state_eval for every element of the parsetree until it's empty
(define eval-rec
  (lambda (parsetree state return) 
    (cond
      ((null? parsetree) (error "Missing return statement"))
      (else (eval-rec (cdr parsetree) (m_state_eval (car parsetree) state return null null return) return))))) ;hey, tail recursion! nice




;------------------------------------------------------------------------------------------
;M_STATE FUNCTIONS

;1: Expressions

;1. m_state_eval --  evaluates expressions. what more do you want from me
(define m_state_eval
  (lambda (expr state return break continue throw)
    (cond
      ((null? expr) (error "Undefined expression"))
      ((eq? (opr expr) '=) (m_state_assign expr state)) ;do this & the next one need continue?
      ((eq? (opr expr) 'var) (m_state_declare expr state))
      ((eq? (opr expr) 'while) (m_state_loop expr state return throw))
      ((eq? (opr expr) 'if) (m_state_cond expr state return break continue throw)) 
      ((eq? (opr expr) 'return) (m_state_return (ret-expr expr) state return))
      ((eq? (opr expr) 'begin) (m_state_block expr state return break continue throw)) ;added for handling blocks
      ((eq? (opr expr) 'continue) (m_state_continue state continue)) ;if the statement is just (continue), then call continue on the current state
      ((eq? (opr expr) 'break) (m_state_break state break))
      ((eq? (opr expr) 'try) (m_state_try expr state return throw))
      ((eq? (opr expr) 'throw) (m_state_throw expr state return throw))
      (else (error "Invalid expression")))))



;2-4: Declaration statements

;2. m_state_declare -- the state after declaring a variable. takes a declaration statement decl and the current state state and gives the new value of the state.
(define m_state_declare
  (lambda (decl state)
    (cond
      ((eq? (decl_kind decl) 'dec_only) (m_state_dec_only decl state))
      ((eq? (decl_kind decl) 'dec_init) (m_state_dec_init decl state))
      (else (error "Interpreter error"))))) ;it shouldn't be entering this fcn unless we know it's a variable declaration, and variable declarations are only of those two kinds

;3. m_state_dec_only -- gives the state after a 'dec_only declaration statement decl and the current state state
(define m_state_dec_only
  (lambda (decl state)
    (cond
      ((eq? (lookup (varname decl) state) 'not_found) (state-add-val (varname decl) 'error state)) 
      (else (error "Variable already declared"))))) ;don't want to allow a variable to be declared again.
;to change to boxes, change state-add-val, deprecate state-remove, add state-change-val.

;4. m_state_dec_init -- gives the state after a 'dec_init declaration statement decl and the current state state
(define m_state_dec_init
  (lambda (decl state)
    (cond
      ((eq? (lookup (varname decl) state) 'not_found) (state-add-val (varname decl) (m_value_expr (assign_expr decl) state) state))
      (else (error "Variable already declared")))))



;5: Assignment statements

;5. m_state_assign -- gives the state after an assignment statement. takes the assignment statement, assign and the current state, state
(define m_state_assign
  (lambda (assign state)
    (cond
      ((eq? (lookup (varname assign) state) 'not_found) (error "Variable not initialized"))
      (else (state-change-val (varname assign) (m_value_expr (assign_expr assign) state) state)))))
      ;(else (state-add-val (varname assign) (m_value_expr (assign_expr assign) state) (state-remove (varname assign) state))))))



;6: While loops

;6. m_state_loop evaluates while loops - this is also tail recursive, which is neat
;(define m_state_loop
;  (lambda (expr state return)
;    (if (m_value_expr (condition expr) state) (m_state_loop expr (m_state_eval (loop-body expr) state return) return) state)))

;the break continuation goes just before the loop starts
;the continue continuation goes just before the loop condition evaluates

;6a. m_state_loop -- evaluates while loops, adding the break continuation
(define m_state_loop
  (lambda (expr state return throw)
    (state-removelayers-until ;remove layers until you hit as many as were in the state before entering the loop
     (call/cc
      (lambda (break)
       (m_state_loop-cc expr state return break (numlayers state) throw))) (numlayers state))))

;6b. m_state_loop-cc -- evaluates while loops. should be used inside of m_state_loop.
(define m_state_loop-cc
  (lambda (expr state return break nlayers throw)
       (if (m_value_expr (condition expr) state)
           (m_state_loop-cc expr (state-removelayers-until (call/cc (lambda (continue) (m_state_eval (loop-body expr) state return break continue throw))) nlayers) return break nlayers throw)
           (state-removelayers-until state nlayers))))



;7-9: If statements

;7. m_state_cond -- evaluates if/else statements - the m_state_eval can call m_state_cond, which allows for nested statements
(define m_state_cond
  (lambda (expr state return break continue throw)
    (if (eq? (cond-type expr) 'if_only)
        (m_state_if_only expr state return break continue throw)
        (m_state_if_else expr state return break continue throw))))

;8. m_state_if_only -- evaluates an if statement that doesn't have an else
(define m_state_if_only
  (lambda (expr state return break continue throw)
    (if (m_value_expr (condition expr) state)
        (m_state_eval (then-stmt expr) state return break continue throw)
        state)))

;9. m_state_if_else -- evaluates an if-else statement
(define m_state_if_else
  (lambda (expr state return break continue throw)
    (if (m_value_expr (condition expr) state)
        (m_state_eval (then-stmt expr) state return break continue throw)
        (m_state_eval (else-stmt expr) state return break continue throw))))



;10-13: Continuations: return, break, continue, throw statements

;10. m_state_return --  returns the value of the expression given.
(define m_state_return
  (lambda (expr state return)
    (cond
      ((eq? (m_value_expr expr state) #t) (return 'true))
      ((eq? (m_value_expr expr state) #f) (return 'false))
      (else (return (m_value_expr expr state))))))

;11. m_state_break -- breaks at the point given, returning the current state to outside the current loop
(define m_state_break
  (lambda (state break)
    (if (null? break)
        (error "Invalid use of break")
        (break state))))

;12. m_state_continue -- goes to the next loop iteration instead of continuing to evaluate the loop body. returns the current state to just before the loop condition is checked.
(define m_state_continue
  (lambda (state continue)
    (if (null? continue)
        (error "Invalid use of continue")
        (continue state))))

;13. m_state_throw

(define m_state_throw
  (lambda (expr state return throw)
    (cond
      ((equal? return throw) (error "Improper use of throw"))
      (else (throw (list expr state))))))



;14-15: Blocks

;14. m_state_stmt_list -- evaluates the state after executing a list of statements slist
(define m_state_stmt_list
  (lambda (slist state return break continue throw)
    (cond
      ((null? slist) state)
      (else (m_state_stmt_list (cdr slist) (m_state_eval (car slist) state return break continue throw) return break continue throw))))) ;not tail recursive!!
;15. m_state_block -- evaluates the state after executing a block statement
(define m_state_block
  (lambda (block state return break continue throw)
    (state-removelayer (m_state_stmt_list (block-slist block) (state-addlayer state) return break continue throw))))

;16-19: Try/Catch

;16. m_state_try -- wrapper for the three variants of try blocks

(define m_state_try
  (lambda (try state return throw2)
      (cond
        ((and (is-catch? try) (is-finally? try)) (m_state_try_both try state return throw2))
        ((is-catch? try) (m_state_try_catch try state return throw2))
        (else (m_state_try_finally try state return throw2)))))

;17. m_state_try_catch -- implements try/catch blocks basically through dark magic, this took three hours to code and i understand none of what i wrote

(define m_state_try_catch
  (lambda (try state return throw2)
    (m_state_catch (try_catch try)
      (call/cc
        (lambda (throw)
          (m_state_eval (try_try try) state return null null throw)))
      return throw2 (numlayers state))))

;18. m_state_try_finally -- no continuation here! boring; anyway yeah this does try blocks with no catch, so like. blocks. cool.

(define m_state_try_finally
  (lambda (try state return throw2)
    (m_state_eval (try_finally try) (m_state_eval (try_try try) state return null null return) return null null throw2)))

;19. m_state_try_both -- uses m_state_try_catch and m_state_try_finally to check when all 3 are present

(define m_state_try_both
  (lambda (try state return throw2)
    (m_state_eval (try_finally try) (m_state_try_catch (rmfinally try) state return throw2) return null null throw2)))

;20. m_state_catch -- cleans up the state from being thrown out of the block, evaluates the state for the catch statement, and helpfully returns the original state if it didn't catch anything
;yes, the pair implementation is weird, but there's no other way i could think to pass two variables with one continuation so you do what you gotta do

(define m_state_catch
  (lambda (try pair return throw2 layers)
    (cond
      ((threw? pair) (m_state_eval (catch_catch try (catch-var try) (pair-val pair)) (state-removelayers-until (pair-state pair) layers) return null null throw2))
      (else (pair-state pair)))))

;------------------------------------------------------------------------------------------
;M_VALUE FUNCTIONS

;1-3: Boolean and number values

;1. m_value_val -- binds <value>s to their meanings
(define m_value_val
  (lambda (val)
    (cond
      ((eq? val 'error) (error "Variable not assigned"))
      ((number? val) (m_value_number val))
      (else (m_value_boolean val)))))

;2. m_value_number -- binds numbers to their meanings
(define m_value_number
  (lambda (num)
    num))

;3. m_value_boolean -- binds boolean values true and false to their meanings
(define m_value_boolean
  (lambda (boolean)
    (cond
      ((eq? boolean 'false) #f)
      ((eq? boolean 'true) #t)
      ((boolean? boolean) boolean) ;if it's already #t or #f, it's good to go.
      (else (error "Invalid boolean value")))))



;4: Variables

;4. m_value_var -- gets the value of a variable binding
;(define m_value_var
;  (lambda (var state)
;    (if (eq? (lookup var state) 'not_found)
;        (error "Variable not found")
;        (m_value_val (lookup var state)))))

;4. m_value_var -- gets the value of a variable binding. UPDATED FOR BOX IMPLEMENTATION.
(define m_value_var
  (lambda (var state)
    (if (eq? (lookup var state) 'not_found)
        (error "Variable not found")
        (m_value_val (unbox (lookup var state))))))


;5-6: Expressions

;5. m_value_expr -- finds the value of an expression, including variables and integer/boolean values
;<expr> -> <op><expr><expr>|<var>|<value>
;it checks to see which m_value expression is applicable (operator expression, variable, integer, boolean) and passes it off to that fcn
(define m_value_expr
  (lambda (expr state)
    (cond
      ((list? expr) (m_value_opr_expr expr state))
      ((number? expr) (m_value_number expr))
      ((booleanval? expr) (m_value_boolean expr))
      (else (m_value_var expr state))))) ;assumes that if expr not a list, number, or boolean, then it's  a variable

;6. m_value_opr_expr -- finds the value of an expression with an operator
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
      ((eq? (opr expr) 'throw) (m_value_expr (op1 expr) state))
      (else (error "Operator not recognized")))))
;^definitely not tail recursive or continuation-passing.





;------------------------------------------------------------------------------------------
;HELPER FUNCTIONS

;1-4: State construction and management of structure -- UPDATED FOR LAYERS
;LAYERED STATE FORMAT:
;     (     ( (var5 var6 ... )(val5 val6 ... ) )
;           ( (var3 var4 ... )(val3 val4 ... ) )
;           ( (var1 var2 ... )(val1 val2 ... ) ) ...  )

;1. state_init -- gives the initial state structure for an empty state.
(define state_init
  (lambda ()
    (cons (state-emptylayer) '()))) ;make a new layer and add it to an empty list

;2. state-emptylayer -- makes a new empty layer for the state
(define state-emptylayer
  (lambda ()
    '(()()) ))

;3. state-addlayer -- adds a new empty layer to the top/front of the state
(define state-addlayer
  (lambda (state)
    (cons (state-emptylayer) state)))

;4. state-removelayer -- removes the top/front layer of the state
(define state-removelayer
  (lambda (state)
    (cdr state)))

;5. numlayers -- gives how many layers are in the state
(define numlayers
  (lambda (state)
    (length state)))

;6. state-removelayers-until -- removes layers from the state until a target number of layers ("goal") has been reached.
(define state-removelayers-until
  (lambda (state goal)
    (cond
      ((< (numlayers state) goal) state)
      ((eq? (numlayers state) goal) state)
      (else (state-removelayers-until (state-removelayer state) goal)))))


;5-9: Parts of the state -- UPDATED FOR LAYERS

;5. top_layer -- gets the top layer of the state
(define top_layer
  (lambda (state)
    (car state)))

;6. var_list -- gets the list of variable names in a state layer
(define var_list
  (lambda (layer)
    (car layer)))

;7. value_list -- gets the list of value names in a state layer
(define value_list
  (lambda (layer)
    (cadr layer))) ;car of cdr of state is the second list in state.

;8. first_var -- gets the first variable in a state layer
(define first_var
  (lambda (layer)
    (car (var_list layer))))

;9. first_value -- gets the first value in a state layer
(define first_value
  (lambda (layer)
    (car (value_list layer))))

;10. layer_tail -- gets the tail end of a state layer: the layer minus the first variable and its value
(define layer_tail
  (lambda (layer)
    (cond
      ((null? (var_list layer)) (error "State construction error"))
      ((null? (value_list layer)) (error "State construction error"))
      (else (list (cdr (var_list layer)) (cdr (value_list layer)))))))

;11. state_tail -- gets the tail end of a state: the state minus the first layer
(define state_tail
  (lambda (state)
    (cdr state)))




;11-13bii: Variable management -- UPDATED FOR LAYERS

;Tested 3/8/18 -- (lookup 'x '(((a b c)(1 2 3)) ((f x e)(8 #f 10)) ((c d x)(1 4 #t)))) ==> #f as expected
; (lookup 'd '(((a b c)(1 2 3)) ((f x e)(8 #f 10)) ((c d x)(1 4 #t)))) ==> 4 as expected
;11a. lookup -- lookup variable in the state and return its 'value'
;if want the value of its value, need (m_value (lookup var state))
;Currently build for a state formatted like (  ((var1 var2 ...)(val1 val2 ...)) ((var3 var4 ...)(val3 val4 ...)) ...)
(define lookup
  (lambda (var state)
    (cond
      ((null? state) 'not_found) ;if the state is '() meaning all layers have been searched, then variable wasn't found.
      ((eq? 'not_found (lookup-in-layer var (top_layer state))) (lookup var (state_tail state))) ;if looking up the variable in the layer results in 'not_found, check the rest of the state
      (else (lookup-in-layer var (top_layer state)))))) ; if looking up in the top layer doesn't result in 'not_found, then return the found variable binding

;11b. lookup-in-layer -- lookup variable in a given state layer and return its 'value'
;Currently built for a state layer formatted like ((var1 var2 var3 ...) (value1 value2 value3))

(define lookup-in-layer ;we previously called this just "layer"
  (lambda (var layer)
    (cond
      ((null? (var_list layer)) 'not_found);(error "Undeclared variable")) ;not sure how to specify which variable
      ((and (eq? var (first_var layer)) (null? (value_list layer))) (error "State construction error"))
      ((eq? var (first_var layer)) (first_value layer))
      (else (lookup-in-layer var (layer_tail layer)))))) ;Note that lookup-in-layer is tail recursive


;Tested 3/8/18 -- (state-add-val 'x 10 '(((a b c)(1 2 3)) ((f x e)(8 #f 10)) ((c d x)(1 4 #t)))) ==> (((x a b c) (10 1 2 3)) ((f x e) (8 #f 10)) ((c d x) (1 4 #t))) as expected
;12. state-add-val -- adds a variable (and its value) to the state - returns a state
;above, we use 'error for the value if the variable is declared but not initialized
;(define state-add-val
;  (lambda (var value state)
;    (cons (list (cons var (var_list (top_layer state))) (cons value (value_list (top_layer state)))) (state_tail state))))
;add the variable and its value to the top layer of the state, then cons the modified top layer onto the rest of the state


;12. state-add-val -- adds a variable and its value to the state, using boxes. for BOX IMPLEMENTATION
(define state-add-val
  (lambda (var value state)
    (cons (list (cons var (var_list (top_layer state))) (cons (box value) (value_list (top_layer state)))) (state_tail state))))
;then add a call to unbox inside m_value_var

;13. state-change-val -- changes the box value of a variable in the state to the new value. note that this fcn isn't recursive, and is for BOX IMPLEMENTATION
(define state-change-val 
  (lambda (var value state)
    (begin
      (set-box! (lookup var state) value) ;lookup the variable's value in the state, which should be a box. set that box to the new value.
      state)))




;DEPRECATED!!!
;Tested 3/8/18 -- (state-remove 'x '(((a b c)(1 2 3)) ((f x e)(8 #f 10)) ((c d x)(1 4 #t)))) ==> (((a b c) (1 2 3)) ((f e) (8 10)) ((c d x) (1 4 #t))) as expected.
;13ai. state-remove -- cover for state-remove-cps
(define state-remove
  (lambda (var state)
    (state-remove-cps var state (lambda (v) v))))

;13aii. state-remove-cps -- removes a variable (and its value) from the state - returns a state
(define state-remove-cps ;NOT YET FULLY TAIL RECURSIVE -- using lookup-in-layer takes a second stack frame. 
  (lambda (var state return)
    (cond
      ((null? state) (return state))
      ;only want to remove the first instance
      ((eq? (lookup-in-layer var (top_layer state)) 'not_found) (state-remove-cps var (state_tail state) (lambda (v) (cons (top_layer state) v))))       ;if the variable isn't in the top layer
      (else (state-remove-from-layer-cps var (top_layer state) (lambda (v1 v2) (return (cons (list v1 v2) (state_tail state))))))))) ;if the variable is in the top layer, remove it from the top layer and cons that onto the rest of the state -- don't remove the variable from the rest of the state!!
      ;(else (cons (state-remove-from-layer var (top_layer state)) (state_tail state)))   << non-tail-recursive else case

;13bi. state-remove-from-layer -- cover for state-remove-from-layer-cps
(define state-remove-from-layer
  (lambda (var layer)
    (state-remove-from-layer-cps var layer (lambda (v1 v2) (list v1 v2)))))

;13bii. state-remove-from-layer-cps -- removes a variable (and its value) from a state layer state - returns a state
;return = (lambda (v1 v2) (list v1 v2))
(define state-remove-from-layer-cps
  (lambda (var layer return)
    (cond
      ((null? (var_list layer)) (return '()())) ;THIS MIGHT BE WHERE WE DIDN'T HAVE ABSTRACTION FOR THE STATE
      ((null? (value_list layer)) (error "State construction error"))
      ((eq? var (first_var layer)) (return (var_list (layer_tail layer)) (value_list (layer_tail layer))))
      (else (state-remove-from-layer-cps var (layer_tail layer) (lambda (vars values) (return (cons (first_var layer) vars) (cons (first_value layer) values ))))))))



;14-18: If/While statements --

;14. loop-body -- gets the loop body to be executed - functional equivalent of op2
(define loop-body
  (lambda (while-stmt)
    (caddr while-stmt)))

;15. condition -- given an if/else or while statement, gets the conditional expression to be evaluated - functional equivalent of op1
(define condition 
  (lambda (stmt)
    (cadr stmt)))
;16. then-stmt -- gets the statement to be done in the case the conditional evaluates to #t - functional equivalent of op2
(define then-stmt
  (lambda (if-else)
    (caddr if-else)))
;17. else-stmt -- gets the statement to be done in the case the conditional evaluates to #f
(define else-stmt
  (lambda (if-else)
    (cadddr if-else)))
    
;18. cond-type -- given an if statement if-stmt, determines if it only has a then-statement or if it has both then- and else- statementts 
(define cond-type
  (lambda (if-stmt)
    (if (null? (cdddr if-stmt))
        'if_only
        'if_else)))



;19-22: Expressions

;19. opr -- finds the operator of an expression
(define opr
  (lambda (expr)
    (car expr)))

;20. op1 -- finds the first operand of an expression
(define op1
  (lambda (expr)
    (cadr expr)))

;21. op2 -- finds the second operand of an expression
(define op2
  (lambda (expr)
    (caddr expr)))

;22. sub_expr? -- checks if something is a sub-expression
(define sub_expr?
  (lambda (s)
    (list? s)))



;23-25: Declaration/Assignment statements

;23. varname -- gets the variable name of the left side of a declaration or assignment statement. works for both 'dec_only and 'dec_init declarations, as well as assignment statements.
(define varname
  (lambda (stmt)
    (cadr stmt)))

;24. assign_expr -- takes a declaration or assignment statement stmt; gets the expression whose value is to be assigned to the var on the left hand side. works only for assignment statements and 'dec_init statements (bc in 'dec_only, cddr is null).
(define assign_expr
  (lambda (stmt)
    (caddr stmt))) ;cdr is (name (expr)) and cddr is ((expr)) so caddr is (expr).

;25. decl_kind -- takes a declaration statement and tells you what kind of declaration it is: 'dec_only, which only declares, or 'dec_init, which declares and initializes.
(define decl_kind
  (lambda (decl)
    (cond
      ((null? (cddr decl)) 'dec_only)
      (else 'dec_init))))



;26: Return statements

;26. ret-expr -- gets the expression to be returned from a return statement - functional equivalent of op1
(define ret-expr
  (lambda (ret-stmt)
    (cadr ret-stmt)))



;27: Booleans

;27. booleanval? -- checks if a value is 'true or 'false
(define booleanval?
  (lambda (val)
    (cond
      ((eq? val 'true) #t)
      ((eq? val 'false) #t)
      (else #f))))

;28: Blocks

;28. block-slist -- gives the block statement without the 'begin at the front
(define block-slist
  (lambda (block)
    (cdr block)))

;29-40: Assorted Try/Catch Helper Functions

;29+30: is-catch and is-finally -- let you know if a try block has a catch or a finally block or not

(define is-catch?
  (lambda (try)
    (not (null? (caddr try)))))

(define is-finally?
  (lambda (try)
    (not (null? (cadddr try)))))

;31-33: these functions all create an evaluatable block of commands; try_try creates an evaluatable try body from the try loop, etc.

(define try_try
  (lambda (try)
    (cons 'begin (cadr try))))

(define catch_catch
  (lambda (catch var val)
    (cons 'begin (cons (list 'var var val) (caddr catch)))))

(define try_finally
  (lambda (try)
    (cons 'begin (cadar (cdddr try)))))

;34. try_catch -- this is just a convenience function to simplify the arguments in m_state_catch - you might say this is a confusing name, to which i would say: yes

(define try_catch
  (lambda (try)
    (caddr try)))

;35: rmfinally -- another convenience function; this isn't strictly functional but the functional implementation is a little over the top so

(define rmfinally
  (lambda (try)
    (list (car try) (cadr try) (caddr try))))

;36+37: double? lets you know if there are 2 elements in a list, threw? lets you know if something was thrown by the try block. neat!

(define double?
  (lambda (lis)
    (cond
      ((null? lis) #f)
      ((null? (cdr lis)) #f)
      ((null? (cddr lis)) #t)
      (else #f))))

(define threw?
  (lambda (lis)
    (and (not (null? (cdr lis))) (double? lis))))

;38-40: these functions all grab specific sections of data used in m_state_catch; catch-var finds the variable name stated by catch, whereas pair-val and pair-state extract information from the continuation

(define catch-var
  (lambda (try)
    (caadr try)))

(define pair-val
  (lambda (lis)
    (car lis)))

(define pair-state
  (lambda (lis)
    (cond
      ((threw? lis) (cadr lis))
      (else lis))))