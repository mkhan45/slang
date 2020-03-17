let x = 1
while x < 10 {
   let y = 1
   let row_str = ""
   while y < 10 {
      let product = x * y
      row_str = row_str + "     " + product as String
      y = y + 1
   }
   print(row_str)
   x = x + 1
}
