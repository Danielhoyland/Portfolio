import Data.Char
main = do

    let (a,b) = splitAt 1 "5B" 
    let c =  ord (head b)
    print (c)