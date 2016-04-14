factorial n =
  if n == 0
    then
      1
    else
      n * factorial (n - 1)

inorder lis =
  if (null lis) || ((null . tail) lis)
    then
      True
    else if (head lis) > ((head . tail) lis)
        then
            False
        else
            inorder (tail lis)

main = print (inorder [1,2,2])
