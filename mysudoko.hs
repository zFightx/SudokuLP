sudoku = [5, 3, 0, 0, 7, 0, 0, 0, 0,
          6, 0, 0, 1, 9, 5, 0, 0, 0,
          0, 9, 8, 0, 0, 0, 0, 6, 0,
          8, 0, 0, 0, 6, 0, 0, 0, 3,
          4, 0, 0, 8, 0, 3, 0, 0, 1,
          7, 0, 0, 0, 2, 0, 0, 0, 6,
          0, 6, 0, 0, 0, 0, 2, 8, 0,
          0, 0, 0, 4, 1, 9, 0, 0, 5,
          0, 0, 0, 0, 8, 0, 0, 7, 9]

-- Pega todas as adjacencias do indice i
pegarAdjacencias i s = linha (calcX' i, calcY' i) s ++ coluna (calcX' i, calcY' i) s ++ bloco (calcX' i, calcY' i) s
    where
        coluna (x,_) s' = [ s' !! toIndex(x,y) | y <- [0..8]]                                         -- encontra os adjacente em coluna
        linha (_,y) s' = [ s' !! toIndex(x,y) | x <- [0..8]]                                          -- encontra os adjacente em linha
        bloco (x, y) s' = [ s' !! toIndex (xx + calcX x, yy + calcY y) | xx <- [0..2], yy <- [0..2] ] -- encontra os adjacente do bloco
        
        calcX x' = (x' `div` 3) * 3 -- calcula se esta no bloco 3x3 em colunas
        calcY y' = (y' `div` 3) * 3 -- calcula se esta no bloco 3x3 em linhas

        calcX' i' = i' - 9 * (i `div` 9) -- calcula coordenada x
        calcY' i' = i' `div` 9           -- calcula coordenada y
        toIndex (x, y) = x + y * 9       -- encontra o indice para determinada coordenada


-- Retorna todas as possíveis soluções até o momento para o Index i
solucoes i s 
        | (s !! i) == 0 = [ x | x <- [1..9], not (x `elem` pegarAdjacencias i s) ]  -- todas as solucoes para o indice i
        | otherwise = [s !! i]                                                      -- se ja estiver solucionado, retorna a propria solucao

-- Insere um valor x no indice i
-- Pega todos os elemento ate o indice i
-- concatena com elemento x
-- concatena com os elementos iniciando do indice i+1 ate o final
inserirValor i s x = take i s ++ [x] ++ drop (i + 1) s

-- Encontra proxima casa que contem 0 ( zero )
proximoZero i s 
        | i == 80           = 80                    -- se for a ultima posicao, retorna ela msm
        | s !! (i + 1) == 0 = i + 1                 -- se possui zero, encontrou, logo retorna ela msm
        | otherwise         = proximoZero (i + 1) s -- caso contrario, recursao ate encontrar

-- Recursively try and brute-force solve the board given in s, starting at p,
-- with the set of possible solutions at that point.
-- 80 is the index of the last element in s

solve 80 s []     = []
solve 80 s (x:[]) = inserirValor 80 s x
solve 80 s (x:_)  = []
solve _  s []     = []
solve p s (x:xs)  | solvedNext == [] = solve p s xs
                  | otherwise        = solvedNext
  where solveNext p s = solve (proximoZero p s) s (solucoes (proximoZero p s) s)
        solvedNext    = solveNext p (inserirValor p s x)

solveIt s = solve 0 s (solucoes 0 s)

-- intersperse the element c through-out the string xs
joinWith :: a -> [a] -> [a]
joinWith _ (x:[])  = [x]
joinWith c (x:xs)  = x : c : joinWith c xs

-- Pretty-print the board as a spaced out 9 x 9 square
pPrint [] = []
pPrint s  = spaceOut s ++ pPrint (drop 9 s)
  where showS s    = concatMap show s
        space      = ' '
        newline    = "\n"
        spaceOut s = joinWith space (take 9 (showS s) ++ newline) 