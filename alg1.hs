import Debug.Trace

-- pozycja na planszy (x, y)
data Point = Point Int Int deriving (Show)

-- stan gry, czyli rozklad pozycji poszczegolnych pionkow
-- pierwszy element listy - pozycja wilka
-- potem pozycje owiec
type State = [Point] 

-- drzewo gry reprezentujące możliwe stany gry, składa się kolejno z:
-- stanu gry dla danego węzła
-- poddrzew danego węzła
-- wartości funkcji heurystycznej oceniającej dany węzeł
data GameTree = GameTree State [GameTree] Int deriving (Show)

-- stan gry po wykonaniu ruchu przez wilka (o ile był on możliwy)
-- oprócz stanu planszy zwraca 2 flagi
-- czy wilk wygrał
-- czy owce wygrały
data FinalState = FinalState State Bool Bool

-- funkcje zwracające poszczególne współrzędne
xPoint :: Point -> Int
xPoint (Point x _) = x

yPoint :: Point -> Int
yPoint (Point _ y) = y

--funkcja zwracająca położenie wilka
wolfPos :: State -> Point
wolfPos (x:xs) = x

--funkcja zwracająca położenie owiec
sheepsPos :: State -> [Point]
sheepsPos (x:xs) = xs

-- operator porównania dla punktów
instance Eq Point where
                (Point x1 y1) == (Point x2 y2) = (x1 == x2) && (y1 == y2) 

-- czy można się przesunąć na daną pozycję
isMovePossible :: State -> Point -> Bool
isMovePossible state pos = if x >= 0 && y >= 0 && x <=7 && y <=7 
							then not (elem pos state)
							else False
							where 
								x = xPoint pos
								y = yPoint pos
								
-- na podstawie aktualnego stanu gry zwraca listę pozycji, 
-- w których może znaleźć się wilk w następnym ruchu
possibleWolfMoves :: State -> [Point]
possibleWolfMoves state = filter (isMovePossible state) [(Point x y) | x <- [xWolf + 1, xWolf - 1], y <- [yWolf + 1, yWolf - 1]]
									where 
										wPos = wolfPos state
										xWolf = xPoint wPos
										yWolf = yPoint wPos					
								
-- na podstawie aktualnego stanu wraca listę możliwych stanów po ruchu wilka
possibleWolfStates :: State -> [State]
possibleWolfStates state = [x:sPos | x <- possibleWolfMoves state]	
									where sPos = sheepsPos state
								
	
-- na podstawie aktualnego stanu gry zwraca listę pozycji, 
-- w których mogą znaleźć się owce w następnym ruchu	
possibleSheepsStatesWrapper :: State -> [State]
possibleSheepsStatesWrapper state = possibleSheepsStates (sheepsPos state) state
	
-- na podstawie pozycji owiec aktualnego stanu gry zwraca listę pozycji, 
-- w których mogą znaleźć się owce w następnym ruchu						
possibleSheepsMoves :: [Point] -> State -> [State]	
possibleSheepsMoves [] state = []						
possibleSheepsMoves (s:ss) state = filter (isMovePossible state) [(Point x (ySheep + 1)) | x <- [xSheep - 1, xSheep + 1]] : possibleSheepsMoves ss state
									where 
										wPos = wolfPos state
										xSheep = xPoint s
										ySheep = yPoint s	
										
										
-- na podstawie aktualnego stanu zwraca listę możliwych stanów po ruchu owiec
-- nie dziala	T.T
possibleSheepsStates :: [Point] -> State -> [State]
possibleSheepsStates points state = [wPos:a | a <- possibleSheepsMoves points state]	
									where 
										wPos = wolfPos state
		
-- w ostateczności...															
sheepsStates :: State -> [State]
sheepsStates (s:ss) = trace (show ss) [[s]++[x]++[ss!!1] ++ [ss!!2] ++ [ss!!3] | x <- ((possibleSheepsMoves (s:ss) ss) !! 1)]
						++ [[s]++[ss!!0]++[x] ++ [ss!!2] ++ [ss!!3] | x <- ((possibleSheepsMoves (s:ss) ss) !! 2)]								
						++ [[s]++[ss!!0]++[ss!!1]++[x]++[ss!!3] | x <- ((possibleSheepsMoves (s:ss) ss) !! 3)]
						++ [[s]++[ss!!0]++[ss!!1]++[ss!!2]++[x] | x <- ((possibleSheepsMoves (s:ss) ss) !! 4)]	
											
					
--czy wilk wygrywa przy danym stanie gry
wolfWins :: State -> Bool
wolfWins state = yWolf == 0 
					where
						yWolf = yPoint (wolfPos state)
					
-- czy owce wygrywają przy daym stanie gry					
sheepsWin :: State -> Bool
sheepsWin state = not (wolfWins state) && (countWolfMoves state) == 0
				
-- generuje drzewo stanów gry dla zadanej głębokości, z zacznaczeniem czyja to runda	
generateGameTree :: Int -> Bool -> State -> GameTree
generateGameTree 0 turn state = GameTree state [] 0 -- zamiast 0 bedzie wart funkcji oceny, nizej to samo
generateGameTree depth isWolfTurn state | trace ("Wolf turn " ++ show isWolfTurn ++ " depth " ++ show depth) isWolfTurn = GameTree state (map (generateGameTree (depth-1) False) (possibleWolfStates state)) 0
										| trace ("Wolf turn " ++ show isWolfTurn ++ " depth " ++ show depth) otherwise = GameTree state (map (generateGameTree (depth-1) True) (sheepsStates state)) 0
								
test :: Int -> State -> [GameTree]
test depth state = map (generateGameTree (depth-1) True) (possibleWolfStates state)													
								
-- korzystając z algorytmu min-max wybiera optymalny ruch dla wilka
-- sprawdza też czy po ruchu owiec czasem już nie wygrały
--minmax :: State -> FinalState
--minmax state = 								
								
								
								
distanceWolfToEnd :: State -> Int								
distanceWolfToEnd (w:ss) = 7 - yPoint w		

--liczy sumę odlegosci, ale mozna przerobic na srednią jak potrzeba albo cos innego
--Chebyshev distance - the best for cheess board
distanceWolfToSheep :: State -> Int
distanceWolfToSheep ws = func ws
    where func (w:s:ss)  = max (abs(yPoint s - yPoint w)) (abs(xPoint s - xPoint w)) + func (w:ss)
          func [w]  = 0
			
countSheepPassedDebug :: Num t => State -> t						
countSheepPassedDebug ws = func ws
    where func (w:s:ss) | trace ("branch 4") (yPoint w >= yPoint s ) = 1 + (func (w:ss))
                        | trace ("branch 3") otherwise = func (w:ss)
          func [w]  = trace ("branch 1") 0

countSheepPassed :: Num t => State -> t						
countSheepPassed ws = func ws
    where func (w:s:ss) | (yPoint w >= yPoint s) = 1 + (func (w:ss))
                        | otherwise = func (w:ss)
          func [w]  = 0


isMoreSheepPassed :: State -> Bool
isMoreSheepPassed ws | countSheepPassed ws >= 3 = True
                     | otherwise = False

countWolfMoves :: State -> Int
countWolfMoves state = length (possibleWolfMoves state)

	


valueOfNode ws = 200 * (distanceWolfToEnd ws) - 200 * (distanceWolfToSheep ws) + 300 * (fromEnum(isMoreSheepPassed ws)) + 100 * (countWolfMoves ws) + 1000 * (fromEnum(wolfWins ws)) - 1000 * (fromEnum(sheepsWin ws))
valueOfNode1111 ws = 1 * (distanceWolfToEnd ws) + 1 * (distanceWolfToSheep ws) + 1 * (fromEnum(isMoreSheepPassed ws)) + 1 * (countWolfMoves ws) + 1 * (fromEnum(wolfWins ws)) - 1 * (fromEnum(sheepsWin ws))


--funckja do ruszania owca/wilkiem
movepPointToNPosition :: (Eq t1, Num t1) => t1 -> t -> [t] -> [t]
movepPointToNPosition n nPoint (x:xs) | n == 0 = nPoint:xs
                                      | otherwise = x:movepPointToNPosition (n-1) nPoint xs

--funckja do bezpiecznego ruszania owcą/wilkiem
-- n - numer pozycji od 0 do 4, nPoint - nowy punkt z nowymi współrzędnymi, state- plansza przed ruchem, return - nowa tablica gry z wykonanym ruchem
movepPointToNPositionSafe :: (Num t, Eq t) => t -> Point -> State -> [Point]
movepPointToNPositionSafe n nPoint state | isMovePossible state nPoint ==True = movepPointToNPosition n nPoint state
                                         | otherwise = state


							
								
-- moje testy							
p1 = Point 1 7
p2 = Point 0 0
p3 = Point 4 0 
p4 = Point 2 2
p5 = Point 6 0 
p6 = Point 3 4
s = [p1, p2, p3, p4, p5]
