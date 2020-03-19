fn fib(n: Int) -> Int {
   let retval = 1
   if n > 1 {
      let a = fib(n - 1)
      let b = fib(n - 2)
      retval = a + b
   } 
   return retval
}


let x = fib(10)
