l = [1,3,8]
map show l
:m +Data.list
intersperse ", " $ map show l
p3 = print $ print . concat . (intersperse ", ") . map show

p3' l = print $ init $ tail $show l --dont do it this way 

 int3sting :: [a]->String
int3sting = print . concat . (intersperse ", ") . map show
