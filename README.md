## Toy Language

This project implements a basic interpreted functional programming language called `toylang`. It is designed to demonstrate the inner workings of grammars, parsers, compilers, and interpreters in a functional language space. This language was created with Racket. 

## Features
- **Simple Grammar**: `toylang` has a straightforward grammar that includes basic arithmetic operations, variable creation, conditionals, functions, and function calling.
- **Interpreter**: The language includes an interpreter capable of executing `toylang` code.
- **Compiler**: Additionally, there's a compiler provided which compiles the `toylang` code into a simpler representation.
- **Recursive Functions**: Through the use of the `mk-rc` recursive functions are possible, this function is based on y-combinator logic. Please see comments provided within toylang.rkt file
- **Higher-Level Functions**: `toylang` supports the creation and usage of higher-order functions.
- **Basic Arithmetic Operations**: The language includes basic arithmetic operations such as addition, subtraction, multiplication, and division.
- **Function Creation**: Users can define their own functions using the `fun` keyword, enabling the creation of custom procedures tailored to specific tasks or computations.
- **Variable Creation**: `toylang` allows the creation of variables using the `with` keyword, facilitating the storage and manipulation of values within the variable's scope.

## Usage
1. **Installation**: Clone this repository to your local machine. Make sure to have racket downloaded onto your machine before running this code. Download here: https://download.racket-lang.org/
2. **Running Code**: Navigate to the directory containing the `toylang.rkt` file in your terminal/command prompt. Run the following command:
    ```
    path/to/your/racket toylang.rkt
    ```
    Replace `path/to/your/racket` with the path to your Racket installation. Once you've run this command, you can proceed to the next step to run toylang code directly in
    the console. You may also run the code from within the toylang.rkt file using the run-code function.
4. **Executing Code**: Inside the terminal, you are prompted to enter your program as input, after pressing 'enter' you will see your output below and be prompted
   to enter another piece of code. For example:
    ```scheme
    Enter ToyLang code (enter 'q' to quit): (+ 2 2)
    4
    ```
    
## Example Programs
```scheme
(+ 2 2) 
(* 2 10)
(/ 20 5)
(with (x 10) (if0 x 0 1))
(with (test-func (fun (y) (+ y 1))) (test-func 1))
(with (y 2) y)
```

## Contact
Samuel Johnson - samj944@gmail.com  

Project Link - https://github.com/samdev23/toylang

