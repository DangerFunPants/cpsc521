Things to do
===============

# Something that resembles type classes would be cool to implement 
* add :: Num a ~ a -> a -> a
* add :: a -> a -> a add subject to (Num a)

# Typing recursive functions
* This should be straightforward.

# n-ary recursive functions.
* Should also be easy, just need to do it.

# Tuples

# Named product and sum types.
* Follows on nicely from tuples, also related to type classes
```
data My_Data_Type 
  = Constructor_One Int 
  | Constructor_Two Bool
let v = Constructor_One 5; z = Constructor_Two true 
```

# Convenient function definition syntax
* "func a b c = a + b + c" instead of func = \a. \b. \c. a + b + c

# Explore register and stack based VMs to expand code execution ability.
* Right now execution is just another word for lambda reduction which probably isn't tenable.

# Autocompletion for bindings in the global scope
* If the symbol "add" is defined in the global scope, user should be able to type ":t a" followed by a command see "add" as well as any other defined symbols that share the common prefix "a". 
* This should work for all commands that currently take program source as an argument.

# Bugs
* Expression evaluation in the REPL doesn't work properly
```
-- All the types in the following expressions are infered correctly.
let x = 5
let y = 10
let z = x + y
:exec z
IVal 10
```
* Debug mode doing weird stuff. Seems like state is persisting across debug calls somehow.
