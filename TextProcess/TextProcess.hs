
whitespace = ['\t', '\n', ' ']


-- "  dog" -> ""
-- "dog  cat" -> "dog"
getWord :: String -> String
getWord [] = []
getWord (x:xs)
    | x `elem` whitespace = []
    | otherwise           = x : getWord xs

-- "  dog" -> "  dog"
-- "dog  cat" -> "  cat"
dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
    | x `elem` whitespace = (x:xs)
    | otherwise           = dropWord xs
-- "  dog" -> "dog"
-- "cat  " -> "cat  "
dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
    | x `elem` whitespace = dropSpace xs
    | otherwise           = (x:xs)

-- "  dog cat " -> ["dog", "cat"]
splitWords :: String -> [String]
splitWords = split . dropSpace

-- "  dog cat " -> ["", "dog", "cat"]
split :: String -> [String]
split [] = []
split str = getWord str : (split $ dropSpace $ dropWord str)