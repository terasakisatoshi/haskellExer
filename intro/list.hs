main = do 
    print $ [1,2,3,4,5] !! 3
    print([1,2,3,4,5]!!3)
    print [1..5]
    print $[1,2,3]++[4,5]
    print $ 1:[2..5]
    print $ 1:2:[3..5]
    print $ 1:2:3:4:[5]
    print $ [1..4]++[5]
    print $ reverse [5,4,3,2,1]
    print [x | x<-[1..5]]