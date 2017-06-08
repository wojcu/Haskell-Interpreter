# Haskell-Interpreter
An interpreter for my custom programming language. Written in Haskell.

# C--
The main difference between C/C++ and C-- is the complete lack of named functions.
In C-- every function is treated as a lambda expression and thus can be used as a first class object.

Other important difference is the default implementation for every basic object in C--.
For example, creating a new variable on the function type:
```C
@int(char, string) foo;
```
Automatically creates a dummy function for foo returning the default value of the returned type. In this case the code above is equivalent to
```C
@int(char, string) foo = @int(char, string) {
  int result;
  return result;
};
```
