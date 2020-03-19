let n = 3
while n < 300 {
   let n_sqrt = n^0.5

      let i = 2
      let not_break = True
      let is_prime = True
      while not_break {
         if n % i == 0 {
            not_break = False
            is_prime = False
         }

         if i >= n_sqrt {
            not_break = False
         }
         i = i + 1
      }

   n = n + 1
}
