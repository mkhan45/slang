fn fib(n: Int) -> Int {
   if n > 1 {
      let a = fib(n - 1)
      let b = fib(n - 2)
      return a + b
   } 
   return 1
}


fib(20)
