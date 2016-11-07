module Validuoti
where
message :: String
message = "d1:0d1:v1:x1:xi2e1:yi2ee1:1d1:v1:o1:xi0e1:yi1ee1:2d1:v1:x1:xi1e1:yi0eee"

validate :: String -> Bool
validate str = validateMoves (dropDictionaryEncode str)

validateMoves :: [(Char,Char,Char)] -> Bool
validateMoves moves  
	| (length(filter(\(a,b,c)-> a=='0' && b=='0') moves))>1 = False
    | (length(filter(\(a,b,c)-> a=='0' && b=='1') moves))>1 = False
    | (length(filter(\(a,b,c)-> a=='0' && b=='2') moves))>1 = False
    | (length(filter(\(a,b,c)-> a=='1' && b=='0') moves))>1 = False
    | (length(filter(\(a,b,c)-> a=='1' && b=='1') moves))>1 = False
    | (length(filter(\(a,b,c)-> a=='1' && b=='2') moves))>1 = False	
	| (length(filter(\(a,b,c)-> a=='2' && b=='0') moves))>1 = False
	| (length(filter(\(a,b,c)-> a=='2' && b=='1') moves))>1 = False
	| (length(filter(\(a,b,c)-> a=='2' && b=='2') moves))>1 = False
	| ((length(filter(\(a,b,c)-> c=='x') moves))-(length(filter(\(a,b,c)-> c=='o') moves)))>1 = False
	| ((length(filter(\(a,b,c)-> c=='o') moves))-(length(filter(\(a,b,c)-> c=='x') moves)))>0 = False
	| otherwise = True

dropDictionaryEncode :: String -> [(Char,Char,Char)]
dropDictionaryEncode "" = error "Message is empty"
dropDictionaryEncode ('d' :rest) = parseMoves (take ((length rest) -1) rest) []
dropDictionaryEncode _ = error "Wrong message used"

parseMoves :: String -> [(Char,Char,Char)] -> [(Char,Char,Char)]
parseMoves a [] = 
    let
		v = head (drop 9 a)
		x = head (drop 14 a)
		y = head (drop 20  a)
		rest = drop 23 a
	in parseMoves rest ((x,y,v) : [])
parseMoves ('1' :a) listOfMoves =
	let
		v = head (drop 8 a)
		x = head (drop 13 a)
		y = head (drop 19  a)
		rest = drop 22 a
	in parseMoves rest ((x,y,v):listOfMoves)
parseMoves "" listOfMoves = listOfMoves