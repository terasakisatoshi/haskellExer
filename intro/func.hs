f x = x+1 
a = f 1 

main = do 
    print a
    print (f 1) -- need bracket ()
    print $ f 1 -- you can also use $ unstead of ()