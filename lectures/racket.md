# CS 201 - Racket Notes

# Getting Started in Racket.

The class lectures will generally follow the topics covered below.

*   Why do we use Racket?
*   How many of you have studied Latin?

Racket, like Latin, is not widely used. It is not practical. How many classic scholars have been disappointed on their first visit to Latin America?

Most western languages have roots in Latin. Many languages can trace their syntax and vocabulary to Latin origins. Studying Latin provides a convenient way for studying language itself and the way language evolves.

The same is true of Racket. Racket itself is relatively new, dating from the 1990's. However, Racket is derived from Scheme (1970's) and LISP (1950's). It is one of the oldest and most influential programming languages around. Learning Latin helps you with French and Spanish and English and Italian. You will see elements of Racket in many modern languages, such as Python.

Also, learning a programming language is less like learning a natural language, like French or Chinese, but more like learning to drive a car. It should not matter much what kind of car you use when you learn to drive. (Exception: what is the best kind of car to use when learning to drive a stick shift?) You are learning generic driving skills. Once you learn to drive, it should not matter much what kind of car you are driving. At some point, you will fly somewhere and rent a car that you have never driven before. Within five minutes, you will be on the road becuase most of driving is independent of any specific model of car.

The same is true of programming. The programming skills you learn in this course will transfer to almost any language you use in the future.

> Perlis epigram #26: There will always be things we wish to say in our programs that in all known languages can only be said poorly.

## Evaluating Expressions

We consider rules for the evaluation of expressions in Racket.

### Rule 1: Constants evaluate to themselves.

Constants are expressions like the numbers 18 and -1, the string "hi there!" and the Boolean values #t and #f (for true and false.) As examples of evaluating these expressions in the interaction window in Dr. Racket, we have the following.

```
> 18
18
> -1
-1
> "hi there!"
"hi there!"
> #t
#t
> #f
#f
```

(Numbers are actually a fairly complex subject in most programming languages, so for the time being we will consider only integers, that is, positive, negative, and zero whole numbers.)

The term "application" means procedure call. The syntax of an application is as follows.

    (proc arg1 ... argn)

where proc is an expression (which evaluates to a procedure), and arg1, arg2, ..., argn are expressions (which are evaluated to determine the arguments to the procedure.) The rule for evaluating an application is the following

### Rule 2. An application is evaluated by evaluating the first expression (whose value must be a procedure) and each of the rest of the expressions; the procedure is called with the values of the rest of the expressions as its actual arguments; when the procedure returns a value, that value is the value of the application.

As an example of an application, we have the following.

```
> (+ 18 4)
22
>
```

In the application (+ 18 4), + is an identifier which evaluates to a procedure, namely, the built-in procedure to add numbers. The expressions 18 and 4 are constants, which evaluate to themselves according to rule (1). Then the built-in procedure to add numbers is called with the arguments 18 and 4, and returns the value 22, which becomes the value of the whole application expression. Just how + evaluates to a procedure will be seen shortly. As additional examples of applications, we have the following.

```
> (- 18 4)
14
> (\* 6 3)
18
> (quotient 22 6)
3
> (remainder 22 6)
4
>
```

The identifiers -, \*, quotient, and remainder evaluate to the built-in procedures to (respectively) subtract, multiply, take the integer quotient, and take the integer remainder. (Using the "division algorithm" we divide 6 into 22 getting a quotient of 3 and a remainder of 4, so 22 = 3\*6+4.)

Note that Racket is uncompromising: in an application, the expression for the procedure \*always\* comes first. What happens when (inevitably) we write something like (18 - 4)? We get an error message, something like:

```
> (18 - 4)

application: not a procedure;
expected a procedure that can be applied to arguments
given: 18
arguments: ...:
```

> Note exception: (18 . - . 4)  
> We point this out not as an endorsement, but as a warning, much like telling you about poison ivy or rattlesnakes. You should know that they exist, but steer clear of them.

The first expression after the left parenthesis is 18, which is a number, not a procedure. The error message comes from the code to evaluate an application and is trying to tell us that it expected a procedure but got 18 instead. Feel free to try things out in the interaction window to see what happens -- it will help you to be able to interpret error messages if/when you see them in response to running your own programs.

The third rule is simple but powerful.

### Rule 3. The rules apply recursively.

This means that sub-expressions are evaluated the same way as any other expressions. As an example, consider the following.

```
> (+ (\* 3 6) 4)
22
>
```

The outer set of parenthesis indicate an application. The first expression is +, which evaluates to the built-in procedure to add numbers. The second expression is (\* 3 6), which is another application. We need to find its value to know what the first argument to the addition procedure should be. So (recursively) we evaluate this expression. It is also an application, with first expression \*, which evaluates to the built-in procedure to multiply numbers. The other expressions are 3 and 6, which are constants and evaluate to themselves. Then the multiplication procedure is called with arguments 3 and 6 and returns 18. Now we know the value of the first argument to the addition procedure. The expression for the second argument is 4, which is a constant that evaluates to itself. Now we can call the addition procedure with arguments 18 and 4, and it returns 22, which becomes the value of the whole expression.

So what rule can we use to evaluate an identifier like +, \*, or remainder? We need the concept of an "environment", which is a table with entries consisting of an identifier and its value. The identifier is said to be "bound" to the value in the environment. There is a top-level environment already defined when you start Dr. Racket; it contains identifiers such as +, \*, and remainder, and gives their values as the built-in procedures to add numbers, multiply numbers, and take the remainder of two numbers. We could picture this top-level environment as a table as follows.

| identifier | value |
|-|-|
| + |   built-in addition procedure |
| \* | built-in multiplication procedure |
| remainder | built-in remainder procedure |

(Of course, there are many more than three entries in the top-level environment.) Then the rule for evaluating identifiers can be stated as follows.

### Rule 4. The value of an identifier is found by looking it up in the "relevant" environment.

This is well defined up to the specification of the "relevant" environment. For the moment, there is only one environment we will consider, namely, the top-level environment, so that will be the "relevant" one. Returning to the application (+ 18 4), we see that the first expression in the application, the identifier +, is evaluated by looking up its value in the top-level environment, where its value is found to be the built-in addition procedure.

Can we add entries to the top-level environment? Yes, we can do so using the "special form" whose keyword is **define**. The terminology "special form" means an expression that looks somewhat like an application, but actually has a different evaluation rule. The syntax of define is as follows.

    (define identifier expression)

The evaluation rule for this expression is as follows.

### Rule 5. A define expression adds the identifier to the relevant environment
with a value obtained by evaluating the expression.

If the identifier is already in the relevant environment, its binding is changed to the value obtained by evaluating the expression. A define expression may look a bit like an assignment statement, but that is the wrong way to think about it. You'll use it in your homework primarily to define procedures in Dr. Racket's definitions window. As an example, if we evaluate the following expression:

```
> (define age 18)
>
```

then the identifier age is added to the top-level environment with the value of the expression 18 (namely 18 itself) as its value. So we can then picture the top-level environment as follows.

| identifier | value |
|-|-|
| + |   built-in addition procedure |
| \* | built-in multiplication procedure |
| remainder | built-in remainder procedure |
| age | 18 |

Then we can evaluate age as follows.

```
> age
18
>
```

And we can proceed to use age in other expressions, for example the following.

```
> (define new-age (+ age 4))
> new-age
22
>
```

In this case, another identifier, new-age (note that the dash is part of the identifier), is added to the top-level environment, with the value 22, which is the result of evaluating the expression (+ age 4). In detail, + is evaluated by looking it up in the top-level environment, where its value is found to be the built-in addition procedure. The identifier age is also evaluated by looking it up in the top-level environment, where its value is found to be the number 18. The expression 4 evaluates to itself, and the addition procedure is called with the arguments 18 and 4, and returns the value 22. This value is bound to the identifier new-age in the top-level environment, which we can now picture as follows.

| identifier | value |
|-|-|
| + |   built-in addition procedure |
| \* | built-in multiplication procedure |
| remainder | built-in remainder procedure |
| age | 18 |
| new-age | 22 |

When we evaluate new-age, its value is looked up in the top-level environment and found to be 22. Note that when we quit and restart Dr. Racket, the top-level environment returns to its initial contents, so age and new-age would no longer be in the top-level environment in that case.

## Special Forms

How can we tell a "special form" from an ordinary application expression? They are both enclosed in parentheses, but a "special form" has one of a small number of keywords (eg, define) as the first expression in the list. Note that parentheses in Racket have a rather different function from their use in mathematics, where they can be used for grouping and are sometimes optional. It is important to retrain your intuition so that you do not think of parentheses in Racket as negligible or innocuous.

> The Racket Guide lists [a finite number of special forms.](https://docs.racket-lang.org/ts-reference/special-forms.html) In this course, we will focus on a handful: define, if, cond, and, or, quote, let, let\*, case, struct, and lambda. No big drama.

Try evaluating the expression +, and you will see how Racket represents the built-in procedure +. Try defining \* to be + and see what happens. (Remember that you can restore the initial top-level environment by quitting and re-starting Dr. Racket.)

We are programmers! When are we going to define our own procedures? There is a special form with keyword lambda that causes a procedure to be created. The syntax is as follows.

    (lambda (arg1 ... argn) expression)

The keyword lambda signals that this is a lambda-expression. The (arg1 ... argn) component is a finite sequence of identifiers arg1, arg2, and so on, up to argn, that gives names to the "formal arguments" of the procedure. The final expression is the "body" of the procedure and indicates how to compute the value of the procedure from its arguments. As an example, we can evaluate the following expression.

```
> (lambda (n) (+ n 4))
```

Evaluating this expression creates a procedure of one formal argument, n, that takes a number, adds 4 to it, and returns the resulting sum. In fact, in this case, the procedure is created, but neither applied nor named, so it just drifts off into the ether. We could not only create it, but also apply it, as follows.

```
> ((lambda (n) (+ n 4)) 18)
22
>
```

What happened here? The outer parentheses are an application (after the first left parenthesis there is another left parenthesis, not a keyword.) The first expression, (lambda (n) (+ n 4)), is evaluated, which creates a procedure of one argument that adds 4 to its argument and returns the sum. The second expression, 18, is evaluated (to itself), and the procedure that we just created is called on the argument 18. The procedure adds 4 to 18 and returns 22, which is the value of the application. At least the procedure got applied in this case, but only once, and then it drifted off into the ether. To use a procedure multiple times, we can give it a name, eg, by using the define special form. We'll see more details of procedures, and more examples, in the next lecture.

You may define functions that take a variable number of arguments.

```
> (define x (lambda (a b) (+ a b)))
> (x 3 4)
7
> (define y (lambda n n))
> (y 1 2 3)
'(1 2 3)
> (define z (lambda n (apply + n)))
> (z 3 4)
7
> (z 3 4 5 6)
18
```

We simply follow the lambda keyword with an argument name not enclosed in parens. The apply procedure uses the procedure of its first argument and evaluates the rest of the arguments.

We can also specify functions with at least n arguments.

```
> (define w (lambda (a b . c) (+ a b (apply + c))))
> (w 1 2)
3
> (w 1 2 3 4 5 6)
21
> (w 1)
; w: arity mismatch;
;  the expected number of arguments does not match the given number
;   expected: at least 2
;   given: 1
; \[,bt for context\]
```

Here we define function w to take at least two arguments, but allow more.

See [racket1.rkt](racket1.rkt) for examples of defining racket variables and functions with variable or optional parameters.

## Collatz Conjecture

![](https://imgs.xkcd.com/comics/collatz_conjecture.png)

Want to win a Fields Medal? Solve the [Collatz Conjecture!](https://en.wikipedia.org/wiki/Collatz_conjecture) (aka, Kakutani's Problem).

We define a function (collatz n) (where n is an arbitrary positive integer) which behaves as follows:

*   If n is even, return n/2.
*   If n is odd, return 3n + 1.

Let's write that function in Racket: (collatz n) See [collatz.rkt](collatz.rkt) (Note use of trace and untrace)

Next, we use recursion to define a sequence of Collatz numbers, such the output of each call becomes the input of the next, unless and until you arrive at 1. The conjecture part is to prove that this series will always converge to 1. We define (c-series n) which uses an if statement and a let statement to create a locale variable. The other function, (c-series2 n), does away with the local variable.

More Racket.
------------

*   How can you tell racket to import a file that has changed since the last time you imported it? Just "enter!" it again. It will reload.
    
    We recall the following principles of racket:
    
    *   Constants evaluate to themselves: numbers, strings, booleans (and actually procedures!)
    
    *   There are [many numerical types in racket](https://docs.racket-lang.org/reference/number-types.html) with corresponding predicates:
        *   (number? 1)
        *   (complex? 2+3i)
        *   (real? pi)
        *   (real? +inf.0)
        *   (rational? 1)
        *   (integer? 1)
        *   (integer? +inf.0)
        *   (integer? 2.0)
        *   (exact-integer? 2.0)
        *   (exact-nonnegative-integer? 0)
        *   (exact-nonnegative-integer? -1)
        *   (exact-positive-integer? 0)
        *   (inexact-real? 3.4)
        *   (inexact-real? 3.5)
        *   (flonum? 3.4)
        *   (double-flonum? 3.4)
        *   (double-flonum? 3.4444444444)
        *   (single-flonum? 3.4)
        *   (zero? 0.0)
        *   (positive? 1)
        *   (negative? 1)
        *   (even? 1)
        *   (odd? 1)
        *   (exact? pi)
        *   (inexact? pi)
        *   (inexact->exact pi)
        *   Load [racket2.rkt](racket2.rkt) and execute (demo) (You should figure out how demo works.)
        
        (define (demo)
          (map
           (lambda (lst)
             (list (car lst)
        	   (cadr lst)
        	   '==>
        	   (apply (eval (car lst)) (cdr lst))))
           examples))
        
    
    *   The leftmost form in a list is evaluated as a procedure, and the remaining elements of the list are passed as arguments to the procedure.
    
    *   The last value is the value returned by the procedure.
    
    *   If you want your procedure to return no value, have the tail position be (void ...). This may be useful if your function is called merely for its side-effects, such as input/output.
    
    *   The rules apply recursively: `(* (+ 9 9) (- 10 2))`
    
    *   The value of an identifier is found by looking it up in the relevant environment. This is just a big table. Be careful: You can clobber definitions.
        
        `(define + \*)`
        
    
    *   A define expression adds the identifier to the relevant environment with a value obtained by evaluating the expression.
    
    *   `define` is a **special form** not a procedure. Another special form is `lambda` which allows you to define a procedure without adding it to the environment.
        
        `(lambda (n) (+ n 4))`
        
        creates a procedure which can be applied to arguments:
        
        `((lambda (n) (+ n 4)) 18)`
        
    
    *   Note: lambda expressions are available these days in most programming languages, including java, python, ruby, r, and haskell.
        
    
## Defining Racket Procedures
    
The evaluation of a lambda expression creates a procedure. Above, we saw how to create and apply a (one-use) procedure in a single expression, for example:
    
    > ((lambda (n) (+ n 4)) 18)
    22
    >
    
But we'd like to be able to re-use our procedures; we can do so by using define, as in the following example.
    
    > (define plus-four (lambda (n) (+ n 4)))
    > (plus-four 6)
    10
    > (plus-four -1)
    3
    >
    
We'll now look at the details of what happened here. Recall that there is an initial top-level environment when you start Racket, which we may picture as follows.
    
| identifier | value |
|-|-|
| + |   built-in addition procedure |
| \* | built-in multiplication procedure |
| remainder | built-in remainder procedure |
    
(Here the dots are included to remind you that there are many other entries in the initial top-level environment.) In the above example, when the define special form is evaluated, the identifier plus-four is added to the top-level environment, and its value is the result of evaluating the expression (lambda (n) (+ n 4)), which is a procedure with formal arguments: n, and body: (+ n 4). So the top-level environment becomes:

| identifier | value |
|-|-|
| + |   built-in addition procedure |
| \* | built-in multiplication procedure |
| remainder | built-in remainder procedure |
| plus-four | user procedure with formal arguments: n and body: (+ n 4) |
    
Note that the procedure has not been applied to any arguments yet. When the next expression, (plus-four 6), is evaluated, the procedure we just created is applied, as follows. The left parenthesis is not followed by a keyword, so this is an application. Using the rules for an application, the identifier plus-four is looked up (in the top-level environment) and its value is found to be a procedure -- so far, so good. The rest of the expressions, in this case just 6, are evaluated, and the procedure is called on the values, again, just 6.
    
The process of calling the procedure on its argument can now be understood as follows. A \*new\* local environment is created using the formal arguments of the procedure (here, just the identifier n) and the corresponding actual arguments (here, just the integer 6). We can picture this new local environment as follows.
    
| identifier | value |
|-|-|
| n | 6 |
    
In addition, there is a "search pointer" that points from this environment to the top-level environment, which indicates where to look for the value of an identifier that is not found in this environment. On the blackboard, this is just an arrow labeled "search pointer" pointing from this environment to the top-level environment. In this medium, we will just use text to indicate the search pointer. So, at this point, the whole environment picture is as follows.

**Top Level Environment:**
| identifier | value |
|-|-|
| + |   built-in addition procedure |
| \* | built-in multiplication procedure |
| remainder | built-in remainder procedure |
| plus-four | user procedure with formal arguments: n and body: (+ n 4) |

**Local Environment:**
| identifier | value |
|-|-|
| n | 6 |
    
Now that the local environment is set up, the process of applying the procedure evaluates the body of the procedure, in this case, (+ n 4), in the local environment. Now we can understand the meaning of the "relevant" environment in the rule for evaluating identifiers. An expression is evaluated in a current environment; to find the value of an identifier, we first look in the current environment to see if it has a binding there -- if so, that is its value. If not, then we follow the environment's search pointer (if any) to another environment and see if it has a binding there -- if so, that is its value. If not, then we follow that environment's search pointer (if any) to another environment, and so on. If this process reaches the top-level environment (which has no search pointer) and does not find a binding for the identifier there, then an error message is generated. An example of such an error message follows.
    
    > x
     (..... stuff ..............)    x: undefined;
     cannot reference an identifier before its definition
    
(The "stuff" tells you where in the Racket system the error was detected.)
    
Back to the application in progress: the body of the procedure, that is, the expression (+ n 4), is now evaluated in the local environment just created. The left parenthesis is not followed by a keyword, so this is an application. The first expression, +, is an identifier, and is evaluated by looking it up in the relevant environment. So first we look in the current environment, which is the local environment. It is not there, so we follow the search pointer to the top-level environment, where we find that it has as its value the built-in procedure to add numbers. The expression n is also an identifier, but in this case we find its value in the current environment, namely 6. Finally, 4 is evaluated (to itself) and we call the built-in addition procedure on 6 and 4; it returns 10, which is the value of the expression (+ n 4) in the local environment, and is the value of the application (plus-four 6).
    
Once the application has been evaluated, what happens to the local environment we just created? It is no longer accessible, and become eligible for "garbage collection" or "recycling", which means that the Racket system may reclaim the memory that it used for other purposes. Conceptually, it is as though the local environment is erased immediately after the application completes, so that the environment picture returns to its previous situation, as follows.
    
| identifier | value |
|-|-|
| + |   built-in addition procedure |
| \* | built-in multiplication procedure |
| remainder | built-in remainder procedure |
| plus-four | user procedure with formal arguments: n and body: (+ n 4) |
    
Note that the binding for plus-four in the top-level environment remains as before. If next we evaluate the expression (plus-four -1), the process is repeated analogously, creating a new local environment in which n is bound to -1, with its search pointer pointing to the top-level environment, and the body expression (+ n 4) is evaluated in this local environment and found to have value 3. Once evaluation of this application completes, its local environment is eligible for garbage collection (and may be thought of as erased.)
    
Whew! This is a lot of detail, but it is intended to give you an inside view of how the Racket interpreter works, which in turn will enhance your understanding of functional programming. You will seldom need to think about this level of detail while you are writing your procedures in Racket.
    
### Alternate procedure definition syntax.
    
We now get a bit of "syntactic sugar" -- to let you avoid typing lambda all the time, and to make your code a little prettier, there is an alternate syntax for defining procedures. For example, we could define the plus-four procedure as follows.
    
        (define (plus-four n) (+ n 4))
    
Note that the define keyword is not followed by an identifier, but by a parenthesized list of identifiers. The first one is taken to be the procedure name, and the rest of them are taken to be the formal arguments to the procedure. (If there are more than one formal arguments, they are separated by white space, not commas.) This is syntactic sugar in the sense that it is a little more user-friendly than the previous syntax, but is just an abbreviation for it. Though you won't be typing lambda all the time, you should understand how lambda expressions work.
    
We'll write some more procedures. We'd like a procedure (last-digit n) that returns the last decimal digit of the positive integer n. As an example of its behavior we have the following.
    
    > (last-digit 437)
    7
    >
    
Rather than picturing what happens in the interaction window, we introduce a shorthand (=>) for the concept that an expression evaluates to a value. Thus, we could indicate the above example by writing the following.
    
        (last-digit 437) => 7
    
This is read as follows: the expression (last-digit 437) evaluates to 7. We'll first write it using lambda, and then give the abbreviated definition. Recall that quotient and remainder are built-in procedures giving the quotient and remainder of an integer division. If we divide 437 by 10, we get a quotient of 43 and a remainder of 7, so the remainder of the input and 10 will be exactly the last decimal digit of the input. Hence we can write the last-digit procedure as follows.
    
        (define last-digit
          (lambda (x)
            (remainder x 10)))
    
Note that I've used newlines and indentation to aid the readability of this procedure. Dr. Racket will help with indentation -- if you highlight a region of code and press tab, it will indent the code according to its parenthesis nesting. This can help you find errors in your parenthesis nesting. Note also that I chose to call the formal argument x -- the particular identifiers you choose for formal arguments are not important, except that naming things well will help you program well. In the alternate procedure definition syntax, this could be rewritten as follows.
    
        (define (last-digit x)
          (remainder x 10))
    
### The factorial function.
    
We come to our first recursive procedure definition. (Not counting the collatz series!) Recall from earlier educational experiences the definition of the factorial function, n!, for positive integers.
    
        n!   =    if n = 1 then the value is 1
                  otherwise, the value is n x (n-1)!
    
So, for example, to compute 4!, we have the following.
    
        4!   =    4 x 3!
             =    4 x 3 x 2!
             =    4 x 3 x 2 x 1!
             =    4 x 3 x 2 x 1
             =    24
    
The case n = 1 is a "base case" -- it returns a value (1) without any further references to the factorial function. The other case (n not equal to 1) is a "recursive case" -- we need to compute the factorial function on another value (namely (n-1)) in order to find the value of the factorial function on n.
    
We'd like to write a procedure (factorial n) to compute the value of n! For example, we'd like (factorial 4) => 24. In order to write a procedure based on the definition above, we need two things: a way to test whether the input is equal to 1 or not, and a way to do one thing if it is, and something else if it is not. The testing can be done by using built-in predicates. A "predicate" is just a procedure that returns either #t (for true) or #f (for false). The built-in predicates =, <, >, <=, >= can be used to compare two numbers to determine whether they are equal (=), or the first is less than (<), greater than (>), less than or equal to (<=), or greater than or equal to (>=), the second. As examples, we have the following. (Recall that the procedure invariably comes first.)
    
        (= 3 4) => #f
        (= (+ 1 3) 4) => #t
        (< 3 4) => #t
        (<= 3 4) => #t
        (<= 4 4) => #t
        (> 3 4) => #f
        (> 4 3) => #t
        (>= 4 3) => #t
        (>= 4 4) => #t
    
Every value in Racket has a type, and there are predicates to test the types of values. For example, the predicate number? tests whether its argument is a number. Note that the question mark is part of the identifier. There is a convention in Racket (and Scheme) that ending the name of a procedure with a ? indicates that it is a predicate, that is, always returns #t or #f. The above predicates expect numbers as their arguments, and return an error message if this is not true. As an example, consider the following.
    
    > (= "hi!" 7)
    (..... stuff .....) 
    =: contract violation
    expected: number?
    given: "hi!"
    argument position: 1st
    other arguments ...:
    
This error message is telling you that the built-in procedure = experienced a "contract violation", which means that its input didn't satisfy some requirement. In this case, it says it was expecting an argument of type number, (indicated by the predicate number?), and was given instead the string "hi!", and that this happened in the first argument position.
    
For testing equality of general Racket values, you can use the predicate equal? For this predicate we have the following.
    
    > (equal? "hi!" 7)
    #f
    >
    
You get no error message, just the answer that "hi!" and 7 are not equal.
    
Now we know how to test whether the input n is equal to 1, namely the expression (= n 1). But we also need to be able to do one thing if it is and something else if it isn't. For this we can use the special form if. The syntax of if is as follows.
    
        (if expression1 expression2 expression3)
    
The keyword is if, and it must be followed by exactly three expressions. The evaluation rule is as follows. The condition, expression1, is evaluated. If the value is not #f, then expression2 is evaluated and its value returned. If the value is #f, then expression3 is evaluated and its value returned. Notice that (unlike in an application), we \*don't\* evaluate all three expressions: either we evaluate expression1 and expression2, or we evaluate expression1 and expression3, but not both. (This is the "coffee or tea" exclusive or, as opposed to the "milk or sugar" inclusive or.)
    
With this new special form, we can finally write (factorial n) as follows.
    
        (define factorial
          (lambda (n)
            (if (= n 1)
                1
                (\* n (factorial (- n 1))))))
    
Using the alternate procedure definition syntax, we have the following.
    
        (define (factorial n)
          (if (= n 1)
              1
              (\* n (factorial (- n 1)))))
    
Note that the "condition", expression1, in the if expression is (= n 1), which tests whether n is equal to 1. The "then case", expression2, is just 1, which is the value that is returned when n is equal to 1. The "else case", expression3, is an expression that multiplies n by the result of a recursive call to the factorial procedure on the value of (- n 1), which is 1 less than n. Thus, this procedure definition mirrors the original definition we gave for n! above.
    
To understand how (factorial 4) => 24, we see that (factorial 4) first has to compute (factorial 3) and multiply it by 4. And (factorial 3) has to compute (factorial 2) and multiply it by 3. And (factorial 2) has to compute (factorial 1) and multiply it by 2. But in the application (factorial 1), the value of the argument n is 1, so we reach the base case, and (factorial 1) simply evaluates to 1. Then (factorial 2) can multiply 1 by 2 and evaluate to 2. Then (factorial 3) can multiply 2 by 3 and evaluate to 6. Finally, (factorial 4) can multiply 6 by 4 and evaluate to 24.
    
See [racket3.rkt](racket3.rkt) and try out the trace facility.
    
### More Racket
    
*   Can you undefine a value in racket? That is, can you remove it from the namespace? The answer appears to be no, however, I am willing to hear suggestions. [Here is the stackoverflow rationale.](https://stackoverflow.com/questions/3487138/how-to-undefine-a-variable-in-scheme)
    
> Perlis epigram #23: To understand a program you must become both the machine and the program.
    
#### Combining Boolean values.
    
There is a built-in procedure (not) and two special forms (and, or) that can be used to combine Boolean values. The syntax of not is as follows.
    
        (not exp)
    
The expression exp is evaluated, and if its value is not #f, then the value #f is returned; if its value is #f, then the value #t is returned. As examples, we have the following.
    
        (not #f) => #t
        (not #t) => #f
        (not (= (+ 1 3) (+ 2 2))) => #f
        (not (> 2 4)) => #t
    
Non-Boolean values are treated as not #f, so, for example (not 13) => #f.
    
#### The special forms: and, or.
    
Recall that the meaning of "special form" is that the rules of evaluation are not those of an application. The syntax of these two special forms is as follows.
    
        (and exp1 exp2 ... expn)
        (or exp1 exp2 ... expn)
    
Each takes an arbitrary finite number of expressions as arguments, and evaluates them in order, left to right, possibly stopping early. The evaluation rule for and: first exp1 is evaluated, and if its value is equal? to #f, then the value #f is returned for the whole and expression -- in this case, no further expressions are evaluated. If the value of exp1 is not equal? to #f, then exp2 is evaluated, and if its value is equal? to #f, then the value #f is returned for the whole and expression (and no further expressions are evaluated.) If this process continues until expn is evaluated, its value is simply returned as the value of the whole and expression. We have the following examples.
    
        (and #f #f) => #f
        (and #t #f) => #f
        (and #f #t) => #f
        (and #t #t) => #t
        (and (= (+ 1 2) 3) (> 4 3)) => #t
        (and (> 4 3) (< 12 6)) => #f
        (and (= (+ 1 1) 2) (> 4 3) (< 6 12)) => #t
    
Non-Boolean values are treated as not #f, so we have (and 1 2 13) => 13. (This is because 1 and 2 are not equal? to #f, so we return the value of the last expression, which is 13.) This last kind of behavior is sometimes convenient, but also confusing and somewhat deprecated.
    
The special form or is analogous to the special form and, but is looking for the first not #f value it can find, evaluating expressions left to right. When it finds a not #f value, it returns that value as the value of the whole or expression (not evaluating any further expressions.) If all the expressions evaluate to #f, then #f is returned as the value of the or expression. Some examples follow.
    
        (or #f #f) => #f
        (or #t #f) => #t
        (or #f #t) => #t
        (or #t #t) => #t
        (or (= (+ 1 1) 2) (< 4 3))) => #t
        (or (= 2 3) (> 6 7) (<= 12 6)) => #f
    
Once again, because non-Boolean values are treated as not #f, we have the following behavior: (or 1 2 13) => 1. (This is because the first expression evaluated, namely 1, is not #f, so its value is returned as the value of the or expression.)
    
Edge cases for the special forms: and, or. What happens when we evaluate (and) and (or)?
    
    > (and)
    #t
    > (or)
    #f
    >
    
These expressions do not result in errors when they are evaluated; why are the values chosen reasonable? If we think of and, or as operating on Boolean values, then #t makes sense as an initial value for and: we keep evaluating expressions, combining them with the current value, until either the value becomes #f (which is then the final value) or we run out of expressions (and the final value is #t). Dually, #f makes sense as an initial value for or. For the same reason, these are the mathematical conventions for AND and OR over a set of expressions -- if the set is empty, AND returns true, OR returns false.
    
#### Lists.
    
A list is a finite sequence of values. There is a list of no values, the empty list, which can be represented as '() or as the keyword **empty**. To determine whether an expression is equal? to the empty list, we can use either of the following built-in predicates.
    
        (empty? exp)
        (null? exp)
    
Each of these evaluates the expression exp, and if its value is equal? to '(), then the value #t is returned; otherwise, the value #f is returned. For example we have the following.
    
        (empty? '()) => #t
        (empty? empty) => #t
        (null? empty) => #t
        (null? 13) => #f
        (empty? "") => #f
        (empty? "this is also not the empty list") => #f
    
Here are examples of lists with a nonzero number of elements; each one is given as its "quoted" representation -- note the leading single quote. (The special form quote, abbreviated as a leading single quote, causes its argument not to be evaluated.) First, we have a list with one element, the number 17.
    
        '(17)
    
Next, a list with two elements, the number 17 followed by the number 24 -- remember that order matters in a list.
    
        '(17 24)
    
The separator for the two elements is "whitespace" -- blanks, tabs, newlines and the like. Another list, with three elements, 17 followed by 24 followed by 6.
    
        '(17 24 6)
    
The elements of a list do not have to be of the same type; here is another list with three elements, a number, a string and a Boolean.
    
        '(17 "hi!" #f)
    
The elements of a list may themselves be lists, as in the following example.
    
        '((1 2) (3) 4)
    
This list has three elements: the list '(1 2) followed by the list '(3) followed by the number 4. Lists may be nested within lists to arbitrary depth.
    
#### Selectors and constructors.
    
When we have a compound data structure, we expect to be able to extract parts of it: the procedures that do that are "selectors". We also expect to be able to assemble parts into a compound data structure: procedures that do that are "constructors". There are two basic selectors for lists: one to return the first element of a non-empty list, and one to return the rest of the elements of a non-empty list, without the first one. Each selector has two names: the LISP-historical one, and one that is considerably more mnemonic. The procedure to return the first element of a list is (first lst) or (car lst), and the procedure to return a list of all the elements except the first element is (rest lst) or (cdr lst). Here are examples, using both names for both selectors.
    
        (first '(17 24 6)) => 17
        (car '(17 24 6)) => 17
        (rest '(17 24 6)) => '(24 6)
        (cdr '(17 24 6)) => '(24 6)
        (first (rest '(17 24 6))) => 24
        (car (cdr '(17 24 6))) => 24
    
Compositions of car and cdr procedures (up to some limit, 5?) have "syntactic sugar" in the form of abbreviations, so that (car (cdr lst)) can be abbreviated (cadr lst). Note that in a list of at least 2 elements, cadr returns the second element of the list. Racket has (also as "syntactic sugar") built-in procedures second, third, ... (up to some limit), so that (second '(17 24 6)) => 24. The procedure name second is generally easier for humans to process than cadr.
    
> CAR => contents of the address register  
> CDR => contents of the decrement register  
> [Further discussion](https://news.ycombinator.com/item?id=116151)
    
#### Constructors.
    
In the case of lists, there is one basic constructor, whose name is cons. Its syntax is
    
        (cons item lst)
    
If lst is a list and item is any value, then (cons item lst) returns a list equal to lst with item inserted as the first element. As examples, we have the following.
    
        (cons 17 '(24 6)) => '(17 24 6)
        (cons 6 '()) => '(6)
    
This allows us to construct the list '(17 24 6) using three applications of cons, as follows.
    
        (cons 17 (cons 24 (cons 6 '()))) => '(17 24 6)
    
The innermost cons expression evaluates to the list '(6), the middle cons expression adds 24 at the front of that list, to get '(24 6), and the outermost cons expression adds 17 to that list, to get '(17 24 6).
    
Note that cons does not require its second argument to be a list, though that will cover essentially all our uses of cons. If you cons together two numbers, you get a "dotted pair", as in the following example.
    
    > (cons 3 8)
    '(3 . 8)
    >
    
This looks a bit like a list, but the dot (.) is an indicator that it is not -- sometimes such structures are called "improper lists". When it occurs in your program's output, it is generally an indication that you cons'ed an element onto something that is not a list, which is generally an error to be corrected. There is a built-in predicate (list? exp), which returns #t if exp is a list, and #f otherwise. (pair? exp), which returns #t if exp is a list - even an improper one. As examples, we have the following.
    
        (list? '()) => #t
        (list? '(17 24 6)) => #t
        (list? 17) => #f
        (list? (cons 3 '())) => #t
        (list? "hi!") => #f
        (list? (cons 3 8)) => #f
        (pair? (cons 3 8)) => #t
    
As another example, we will construct the list '((1 2) (3) 4) using cons, numbers, and the empty list '(). Because cons adds elements at the front of the list, we construct a list by working backwards. To get the list '(4), we cons 4 to the empty list:
    
        (cons 4 '()) => '(4)
    
We'd like to add the element '(3) to the front of this list. However, this is itself a list, so we need to construct it similarly to '(4), namely, (cons 3 '()). Thus, we can get a list of the 2nd and 3rd elements of our target as follows.
    
        (cons (cons 3 '()) (cons 4 '())) => '((3) 4)
    
We just need to cons the element '(1 2) onto the front of this list, but we need to construct the list '(1 2) in order to do this. To construct the list '(1 2) we use the expression (cons 1 (cons 2 '())). Putting all this together, we have
    
    (cons (cons 1 (cons 2 '())) (cons (cons 3 '()) (cons 4 '())) => '((1 2) (3) 4)
    
Next we look at various forms of recursive procedures dealing with lists as arguments and return values.
    
## Recursion, Glorious, Recursion

See [recursion.rkt](recursion.rkt) and try out the trace facility.
    
[**modulo vs remainder**](https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._remainder%29%29) See [modulo.rkt](modulo.rkt) (Also, quotient vs /)
    
#### Recursive procedures that take lists as arguments and return lists as values.
    
See [racket4.rkt](racket4.rkt)
    
Recall from above that we have list predicates: empty?, null?, list?, list selectors: first, rest, car, cdr, and a list constructor: cons.
    
There is a built-in procedure (length lst) that takes a list lst as its argument and returns the number of top-level elements in lst. For example we have the following.
    
        (length '()) => 0
        (length '(a b c)) => 3
        (length '((1 2) (3 4 5))) => 2
    
We'll write our own recursive version of length, (our-length lst). As we saw, we need one or more base cases. A natural one for an input list is the empty list, '(). If the input list is empty, we can immediately return the value 0 for our-length. We also need one or more recursive cases that "move" the input towards a base case. In the case of an input list, removing one element from the list (by using rest or cdr) is a natural candidate, since it moves towards the empty list as a base case. As we think about how to construct the procedure, it may be helpful to consider an example. Assuming lst => '(17 24 6), we have the following.
    
        (rest lst) => '(24 6)
        (our-length (rest lst)) => 2   (assuming it works!)
        (first lst) => 17
    
In this case, if the recursive call (our-length (rest lst)) returns a correct value, we just need to add 1 to it to get a correct answer for (our-length lst). We don't actually seem to need the value of (first lst) at all. We are led to define the following procedure.
    
        (define (our-length lst)
          (if (empty? lst)
              0
              (+ 1 (our-length (rest lst)))))
    
We can draw a tree of the recursive calls of (our-length '(17 24 6)) to help us understand how this works.
    
        (our-length '(17 24 6))
         /  |     \\
        +   1   (our-length '(24 6))
                 /  |     \\
                +   1    (our-length '(6))
                          /  |     \\
                         +   1    (our-length '())
    
Notice that each successive recursive call has an argument that is one element shorter than its predecessor, until we reach the base case of the empty list '(). Then there is no further recursive call, and the procedure just returns 0, leading to the following sequence of return values (from the bottom up.)
    
        (our-length '(17 24 6)) => 3
         /  |     \
        +   1   (our-length '(24 6)) => 2
                 /  |     \
                +   1    (our-length '(6)) => 1
                          /  |     \
                         +   1    (our-length '()) => 0
    
Note that the if expression tests (empty? lst) -- it would be equivalent to test (null? lst) or (equal? lst '()).
    
This procedure is not tail-recursive, because the value of the recursive call, (our-length (rest lst)) is modified, that is, has 1 added to it, before being returned as the value of (our-length lst).
    
#### Testing whether an element is in a list.
    
There is a built-in procedure (member item lst) to test whether an arbitrary item is equal? to a top-level element of the list lst. If item is not equal? to any of the top-level elements of lst, then this procedure returns #f. If item is equal to some top-level element of lst, then this procedure returns a list with all the elements of lst from the first one equal? to item until the end. As examples, we have the following.
    
        (member 2 '(1 2 3)) => '(2 3)
        (member 2 '(1 2 3 2 1)) => '(2 3 2 1)
        (member 4 '(1 2 3)) => #f
    
Note that this is not a predicate, because it does not always return #t or #f. We'll write (as a recursive procedure) a predicate version of member, namely (member? item lst). If item is equal? to a top-level element of lst, then (member? item lst) evaluates to #t; otherwise, it evaluates to #f. The behavior we want for the examples above is as follows.
    
        (member? 2 '(1 2 3)) => #t
        (member? 2 '(1 2 3 2 1)) => #t
        (member? 4 '(1 2 3)) => #f
    
Because the input lst is a list, a reasonable base case is when lst is an empty list. In this case, lst contains no top-level elements, so item cannot be equal? to one of them, and the procedure should return #f. Thinking about the first example above, we have item => 2 and lst => '(1 2 3).
    
        (first lst) => 1
        (rest lst) => '(2 3)
        (member? item (rest lst)) => #t   (if it returns the correct answer)
    
So there should be a recursive case in which we evaluate (member? item (rest lst)) and return the value that it returns. However, this isn't the only case; we have the case in which item is actually equal? to (first lst). This is another base case, which should return the value #t. We are led to the following procedure definition.
    
        (define (member? item lst)
          (cond
            [(empty? lst) #f]
            [(equal? item (first lst)) #t]
            [else (member? item (rest lst))]))
    
We used a cond expression to avoid a situation of nested if expressions. There are two base cases: lst is empty, and the answer is #f, and the list is non-empty and item is equal? to the first element of lst, when the answer is #t. If neither of these cases applies, then we need to continue to search for item in the rest of the list. The answer (#t or #f) that the recursive call returns will be the answer to whether (member? item lst) should be #t or #f. We can draw the tree of recursive calls for (member? 24 '(17 24 6)) as follows.
    
        (member? 24 '(17 24 6))
            |
        (member? 24 '(24 6))
    
There are no further recursive calls because 24 is equal? to (first lst) in this case, and #t is returned. The return value just propagates upward as follows.
    
        (member? 24 '(17 24 6)) => #t
            |
        (member? 24 '(24 6)) => #t
    
Note that our member? procedure is tail-recursive: the value of the recursive call is returned (unmodified) as the value of the whole procedure.
    
A procedure that takes a list as input and returns a list. Now we write a procedure (double-each lst) that takes a list lst of numbers and returns the list consisting of twice each number in the original list. As an example we have the following.
    
        (double-each '(17 24 6)) => '(34 48 12)
    
Once again, the empty list is a reasonable base case. We have to decide what we want (double-each '()) to be. We choose the result to be '(), which consists of twice each number in '(). Thinking about the example, if lst => '(17 24 6), we have the following.
    
        (first lst) => 17
        (rest lst) => '(24 6)
        (double-each (rest lst)) => '(48 12)  (assuming it works correctly)
    
For the recursive case, we can call (double-each (rest lst)), and get a list containing twice each of the numbers in the rest of the list. To make this into the desired result, all we have to do is to include twice (first lst) at the front of the list. The list constructor (cons item lst) returns the list equal? to lst with item included as the first element. We may now write the complete procedure as follows.
    
        (define (double-each lst)
          (if (null? lst)
              '()
              (cons (* 2 (first lst))
                    (double-each (rest lst)))))
    
Note that we could get by with an if expression because there were only two cases: the base case and the recursive case. We can draw the tree of recursive calls for the application (double-each '(17 24 6)) as follows.
    
        (double-each '(17 24 6))
         /   |    \
       cons  34  (double-each '(24 6))
                  /   |    \
                cons  48  (double-each '(6))
                           /   |    \
                         cons  12  (double-each '())
    
There are no further recursive calls because we reached the base case (lst => '()), which returns '(). Then the returns propagate up the tree as follows.
    
        (double-each '(17 24 6)) => '(34 48 12)
         /   |    \
       cons  34  (double-each '(24 6)) => '(48 12)
                  /   |    \
                cons  48  (double-each '(6)) => '(12)
                           /   |    \
                         cons  12  (double-each '()) => '()
    
Note that this procedure is not tail-recursive, because the value returned by the recursive call is modified (by having twice (first lst) cons'ed on the front) before being returned as the value of the whole procedure.
    
#### Deep recursion.
    
See add-tree in [racket5.rkt](racket5.rkt)
    
So far we have seen recursion on numbers, from n to n-1, or from n to (quotient n 10), and "flat" recursion on lists, which processes (first lst) in some way and does a recursive call on (rest lst). Now we'll consider a procedure that does "deep recursion", in which the procedure is called recursively on sublists, and sublists of sublists, and so on. There is a built-in procedure (flatten value). Here are some examples of it.
    
        (flatten 17) => '(17)
        (flatten '(17 24 6)) => '(17 24 6)
        (flatten '((1 2) (4 (1 2)) 3)) => '(1 2 4 1 2 3)
        (flatten '(() ())) => '()
    
If the argument value is not a list, the return value is a list containing it as the only element. If the argument value is the empty list, the return value is the empty list. However, if the argument value is a non-empty list, the result is obtained by appending the result of flattening (first value) to the result of flattening (rest lst). We'll write our own version of flatten, (o-f value).
    
        (define (o-f value)
          (cond
            \[(not (list? value)) (list value)\]
            \[(empty? value) '()\]
            \[else
              (append (o-f (first value))
                      (o-f (rest value)))\]))
    
Note that here we've used the built-in list constructor list. This takes an arbitrary finite number of arguments, evaluates all of them, and makes a list of the values. Thus, (list value) is equivalent to (cons value '()). Note the contrast with quote ('), in the following.
    
        (list (+ 1 2) (\* 2 3)) => '(3 6)
        '((+ 1 2) (\* 3 6)) => '((+ 1 2) (\* 3 6))
    
In the case of list, the expressions (+ 1 2) and (\* 2 3) are evaluated, and the list of their values returned. In the case of quote ('), evaluation of the sub-expressions of the expression is inhibited, and the value of '((+ 1 2) (\* 3 6)) is a list containing two lists, the first with the identifier '+, the number 1 and the number 2, and the second with the identifier '\*, the number 3 and the number 6.
    
Since we were out of time, it was suggested to try to draw the tree of recursive calls for an application like (o-f '((1 2) 3)). Here it is.
    
              (o-f '((1 2) 3))
           ______________________
           /    |                 \
          /     |                  \
         /      |                   \
        /       |                    \ 
    append  (o-f '(1 2))              (o-f '(3))
           /    |     \               /    |    \
      append  (o-f 1) (o-f '(2))  append (o-f 3) (o-f '())
                     /    |    \
                append (o-f 2) (o-f '()) 
    
All the leaves are base cases (either the empty list or a non-list value), and they are appended and returned up the tree of recursive calls to yield (o-f '((1 2) 3)) => '(1 2 3).
    
Going forward, deep recursion will appear in many guises, to traverse or process more complex recursive data structures. It is your friend. Like it on Facebook. (BTW, Facebook is basically a huge graph which is a mammoth recursive data structure.)