let n = 0

while n < 50 {
   let n_sqrt = n^0.5

   let x = 1
   let not_break = True

   while not_break {
      print(x)
      if n % x == 0 {
         not_break = False
      }

      x = x + 1
   }
}
