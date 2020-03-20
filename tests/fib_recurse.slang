fn fib(n: Int) -> Int {
   if n > 1 {
      let a = fib(n - 1)
      let b = fib(n - 2)
      return a + b
   } 
   return 1
}


let x = fib(7)
print(x)
