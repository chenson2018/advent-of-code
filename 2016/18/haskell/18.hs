iter xs = aux ('.' : xs ++ ".")
  where
    aux (a : b : c : xs) = (if a == c then '.' else '^') : aux (b : c : xs)
    aux _ = []

main = do
  let input = "^.^^^.^..^....^^....^^^^.^^.^...^^.^.^^.^^.^^..^.^...^.^..^.^^.^..^.....^^^.^.^^^..^^...^^^...^...^."
  let rows = iterate iter input
  print $ length $ filter (== '.') $ concat $ take 40 rows
  print $ length $ filter (== '.') $ concat $ take 400000 rows
