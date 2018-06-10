import Debug.Trace

-- pozycja na planszy (x, y)
data Point = Point Int Int deriving (Show)

-- stan gry, czyli rozklad pozycji poszczegolnych pionkow
-- pierwszy element listy - pozycja wilka
-- potem pozycje owiec
type State = [Point]

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
								
								
-- na podstawie aktualnego stanu wraca listę możliwych stanów po ruchu owiec
possibleSheepStates :: [Point] -> State -> [Point]								
possibleSheepStates (s:ss) state = filter (isMovePossible state) [(Point x (ySheep + 1)) | x <- [xSheep - 1, ySheep + 1]] ++ possibleSheepStates ss state
									where 
										xSheep = xPoint s
										ySheep = yPoint s
									
								
distanceWolfToEnd :: [Point] -> Int								
distanceWolfToEnd (w:ss) = 7 - yPoint w		



--liczy sumę odlegosci, ale mozna przerobic na srednią jak potrzeba albo cos innego
--Chebyshev distance - the best for cheess board
distanceWolfToSheep :: [Point] -> Int
distanceWolfToSheep ws = func ws
    where func (w:s:ss)  = max (abs(yPoint s - yPoint w)) (abs(xPoint s - xPoint w)) + func (w:ss)
          func [w]  = 0
			
countSheepPassedDebug :: Num t => [Point] -> t						
countSheepPassedDebug ws = func ws
    where func (w:s:ss) | trace ("branch 4") (yPoint w >= yPoint s ) = 1 + (func (w:ss))
                        | trace ("branch 3") otherwise = func (w:ss)
          func [w]  = trace ("branch 1") 0

countSheepPassed :: Num t => [Point] -> t						
countSheepPassed ws = func ws
    where func (w:s:ss) | (yPoint w >= yPoint s) = 1 + (func (w:ss))
                        | otherwise = func (w:ss)
          func [w]  = 0


isMoreSheepPassed :: [Point] -> Bool
isMoreSheepPassed ws | countSheepPassed ws >= 3 = True
                     | otherwise = False

															
														
								
								
								
								
								
-- --moje testy		
-- p1 = Point 0 7
-- p2 = Point 1 2
-- p3 = Point 5 6 
-- p4 = Point
-- s = [p2, p1, p3]


--moje testy		
p1 = Point 2 2
p2 = Point 4 4
p3 = Point 5 2
p4 = Point
s = [p2, p1,p3,p3]