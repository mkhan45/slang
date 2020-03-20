fn is_prime(n: Int) -> Bool {
   if n == 2 or n == 3 {
      return True
   }

   if n % 2 == 0 or n % 3 == 0 {
      return False
   }

   let n_sqrt = n^0.5

   let i = 5
   while i < n_sqrt {
      if n % i == 0 or (n % (i + 2) != 0) { 
         return False
      }
      i = i + 6
   }

   return True
}

let n = 2
while n < 30 {
   let res = is_prime(n)
   n = n + 1
}
