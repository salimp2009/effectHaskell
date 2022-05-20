module FoldFunctions where


myFoldl :: (t -> a -> t) -> t -> [a] -> t
myFoldl func carryValue lst 
        | null lst = carryValue
        | otherwise =
            let intermediateVal = func carryValue (head lst)
            in myFoldl func intermediateVal (tail lst)

myFoldr :: (a -> t -> t) -> t -> [a] -> t
myFoldr func carryValue lst 
        | null lst = carryValue
        | otherwise = func (head lst) $ myFoldr func carryValue (tail lst)