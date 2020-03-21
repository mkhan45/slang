# slang

Slang is an interpreted language that I've written the first week of Coronabreak. I tried to write it without reading any examples or tutorials to see if I could figure it out myself. In the end, I'm taking a break from it because everything was super hastily written and parsing especially has become a huge pain.

Ideally I was aiming for a more strongly typed Python or an auto-memory-managed Rust.

As far as my goals at the start of the week go, here's what I've got so far:

- [X] Basic type-checked expression evaluation
- [ ] Type casting
- [X] (Nestable) While loops
- [X] Conditionals (elif chains don't work)
- [ ] Functions
  - [X] Single argument functions are ok
- [ ] Comments
  
___

All in all, Slang works pretty ok for super simple scripting. The biggest problems are that:

- Functions only work with a single arument
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
