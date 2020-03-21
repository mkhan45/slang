# slang

Slang is an interpreted language that I've written the first week of Coronabreak. I tried to write it without reading any examples or tutorials to see if I could figure it out myself. In the end, I'm taking a break from it because everything was super hastily written and parsing especially has become a huge pain.

Ideally I was aiming for a more strongly typed Python or an auto-memory-managed Rust.

As far as my goals at the start of the week go, here's what I've got so far:

- [X] Basic type-checked expression evaluation
- [X] Type casting
- [X] (Nestable) While loops
- [X] Conditionals (elif chains don't work)
- [ ] Functions
  - [X] Single argument functions are ok
- [ ] Comments
  
___

All in all, Slang works pretty ok for super simple scripting. The biggest problems are that:

- Functions only work with a single argument
- Expression evaluation doesn't work with functions, e.g `5 + f(6)` evaluates to just `f(6)`
- There's no lists

As I learned after writing it, Slang is super slow because it's a tree walk interpreter as opposed to a bytecode interpreter.

___

## Example

More examples are in the `tests/` and `benches/` folders; eventually I'll add some proper documentation.

This program prints a simple multiplication table
```rust
fn make_table(n: Int) -> String {
   let ret_str = ""

   let row = 1
   while row <= n {
      let col = 1
      while col <= n {
         ret_str = ret_str + "\t" + (row * col) as String
         col = col + 1
      }

      ret_str = ret_str + "\n"
      row = row + 1
   }

   return ret_str
}

print(make_table(10))
```

## Getting Started
There should be Windows and Linux binaries in the releases tab of Github. Otherwise, build with `cargo build --release`.

Slang is super simple. To start it interactively in a REPL, just run the slang binary. To run a script, just run it on the file.

<details><summary>Types</summary>
<p>
  
- Int
- Float
- String
- Bool

</p>
</details> 

<details><summary>Variables</summary>
<p>
  
 Variable assignment:`let x = 5`
 
 The type is inferred, so here x is an Int. `let x = 5.0` would result in x being a Float, and `let x = "5.0"` would make a string.
 
 You can change a variable later without the `let`. E.g:
 ```rust
 let x = 5
 x = 12
 ```
 
 However, you need `let` if you want to change the variable's type:
 ```rust
 let x = 5
 x = 12
 let x = 53.2
 ```

</p>
</details>

<details><summary>Expressions</summary>
<p>

There are a few operators:
- Addition (`+`)
- Subtraction (`-`)
- Multiplication (`*`)
- Division (`/`)
- Exponentiation (`^`)
- Modulus (`%`)
- Casting (`as`)
- Comparison (`>`, `<`, `>=`, `<=`, `==`)
- Boolean operators: (and, or)

Order of operations should hold in this order, from first evaluated to last:
1. Casting
2. Comparison (Less, Greater, LessEqual, GreaterEqual, Equal) and modulus
3. Exponentiation
4. Multiplication, division
5. Addition, Subtraction
6. And, or

Some operations do not work on every type. Here are some expressions in the REPL
```rust
> print(12 + 5)
17
> print(12.3 + 5)
17.3
> print(22 / 5)
4
> print(22.0 / 5)
4.4
> print(22.5 as Int / 5)
4
```

An especially notable case is string concatenation:
```rust
> print("Hello " + 5)
thread 'main' panicked at 'illegal addition', src/expression_eval.rs:113:18

> print("Hello " + 5 as String)
Hello 5
```
</p>
</details>

<details><summary>Functions</summary>
<p>

Function definitions are exactly the same as in Rust, and similar to Python with typehints:
```rust
fn example(n: Int) -> Int
```
Each argument is in the format `$NAME : $TYPE` but I haven't actually added typechecking yet.
The `-> Int` denotes that this function returns an Int. Void functions don't need an arrow.

</p>
</details>

## TODO
more documentation
