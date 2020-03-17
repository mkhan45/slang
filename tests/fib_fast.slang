let a = 1
let b = 1

let i = 0
while i < 24 {
   let c = b
   b = a + b
   a = c
   i = i + 1
}

print(a)
