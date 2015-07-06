sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs