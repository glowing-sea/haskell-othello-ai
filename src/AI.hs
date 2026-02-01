{-|
Module      : AI
Description : AIs for Othello
Copyright   : (c) 2021 Haoting Chen
License     : MIT
-}
module AI where

import Othello

-- | Type of AI functions you can choose to write.
data AIFunc
  = NoLookahead (GameState -> Move)
    -- ^ Simple AIs that do not need lookahead.
  | WithLookahead (GameState -> Int -> Move)
    -- ^ AIs that want to look ahead. The assignment framework will
    -- call the function over and over with increasing integer
    -- arguments @1, 2, 3, ...@ until your AI's time limit is up.

-- | The table of all AIs that your assignment provides. The AI named
-- "default" in this table is the one your tutor will dedicate most of
-- his or her attention to marking.
ais :: [(String, AIFunc)]
ais = [ ("firstLegalMove", NoLookahead firstLegalMove)
      , ("greedy", NoLookahead easyMode)
      , ("default", WithLookahead hardMode)
      , ("hardMode", NoLookahead hardModeFast)
      ]


-- | A very simple AI, which picks the first move returned by the
-- 'legalMoves' function. AIs can rely on the 'legalMoves' list being
-- non-empty; if there were no legal moves, the framework would have
-- ended the game.
firstLegalMove :: GameState -> Move
firstLegalMove st = head (legalMoves st)

-- ========================================================================== --

-- | The follow are the functions for the AI "easyMode" and "default"
-- | Assumptions:
-- The ai is called only when it has any legal move.
-- The game is not over.
-- The game board is 8 x 8.
-- The legal moves given by the function "legalMoves" are really legal.


-- | The main function of the ai easyMode, 
-- which takes a game state and return the best move.
easyMode :: GameState -> Move
easyMode st = bestMove (filterMoves st (legalMoves st)) st False

-- | The main function of the ai default,
-- which takes a game state and return the best move.
-- If the second argument is not 1, then it will look down to the game tree.
hardMode :: GameState -> Int -> Move
hardMode st n = case n of
  1 -> easyMode st
  _ -> bestMove (filterMoves st (legalMoves st)) st True

-- | The main function of the ai hardMode. This ai is same as the ai default, 
-- but it is used only for debug.
hardModeFast :: GameState -> Move
hardModeFast st = bestMove (filterMoves st (legalMoves st)) st True


-- | A rose tree type
data Rose a = Node a [Rose a]

-- | Generate a Othello game tree to a certain depth, given a depth, 
-- a state, and the player of the state.
othelloTree :: Int -> Player -> GameState -> Rose GameState
othelloTree depth expectedPlayer state
  |depth == 1 = Node state []
  |otherwise = case getTurn state of
    GameOver _ -> Node state []
    Turn player
      |player == expectedPlayer -> Node state subTrees
      |otherwise 
      -> 
        Node (switchPlayer state) 
        [othelloTree (depth - 1) (otherPlayer expectedPlayer) state]
  where 
    nextStates = map (implementMove state) (legalMoves state)
    subTrees = 
      map (othelloTree (depth - 1) (otherPlayer expectedPlayer)) nextStates


-- | This function filters any bad move if not all moves are bad.
-- It also filters other moves if there is a good move.
filterMoves :: GameState -> [Move] -> [Move]
filterMoves st moves
  |all badMove moves = moves
  |any goodMove moves = filter goodMove moves
  |otherwise = filter notBadMove moves
  where
    -- Check if a move is bad.
    badMove :: Move -> Bool
    badMove move
      |move == Move (1,1) && head (head b) /= p = True
      |move == Move (6,1) && head b !! 7 /= p = True
      |move == Move (1,6) && head (b !! 7) /= p = True
      |move == Move (6,6) && b !! 7 !! 7 /= p = True
      |otherwise = False
    -- Check if a move is not bad.
    notBadMove :: Move -> Bool
    notBadMove move = not (badMove move)
    -- Check if a move is bad.
    goodMove :: Move -> Bool
    goodMove move
      |move == Move (0,0) = True
      |move == Move (7,0) = True
      |move == Move (0,7) = True
      |move == Move (7,7) = True
      |otherwise = False
    b = getBoard st
    p = Just (getPlayer st)


-- | This function takes a list of moves and return the best move.
-- The thrid argument tells the function whether it should look down to the 
-- tree to find the best move.
bestMove :: [Move] -> GameState -> Bool -> Move
bestMove moves state lookahead = case moves of
  [x] -> x
  _ -> maxScoreMove (map scoreMove moves)
  where
    -- |Given a move and returns the move and its score.
    scoreMove :: Move -> (Move, Int)
    scoreMove move
      |lookahead = (move, miniMaxAB currentPlayer False 
      (othelloTree 6 opponent (implementMove state move)) (-9999) 9999)
      |otherwise = 
        (move, stateEvaluatePlus (implementMove state move) currentPlayer)

    currentPlayer = getPlayer state
    opponent = otherPlayer currentPlayer

-- |Given a list of moves with their scores, it returns the move with the 
-- maximum score.
maxScoreMove :: [(Move, Int)] -> Move
maxScoreMove movesWithRank = case movesWithRank of
  [] -> error "There always at least one move for the ai to choice"
  [x] -> fst x
  (x,xR):(y,yR):zs
    |xR >= yR -> maxScoreMove ((x,xR):zs)
    |otherwise -> maxScoreMove ((y,yR):zs)


-- | Evaluate a game tree, used for testing only.
-- Input: 
-- 1 The AI's player name, 
-- 2 whether the root of the GameState tree is AI (Max)'s turn
-- 3 game tree
-- Output: the score of the tree
miniMax :: Player -> Bool -> Rose GameState -> Int
miniMax currentPlayer isMax tree = case tree of
  Node state [] -> stateEvaluate state currentPlayer
  Node _ childrenStates
    |isMax -> maximum (map (miniMax currentPlayer False) childrenStates)
    |otherwise -> minimum (map (miniMax currentPlayer True) childrenStates)

-- | Evaluate a game tree.
-- Input: 
-- 1 The AI's player name, 
-- 2 whether the root of the game tree is AI (Max)'s turn
-- 3 game tree
-- 4 Initial alpha value (-9999)
-- 5 Initial beta value 9999
-- Output: the score of the GameState tree
miniMaxAB :: Player -> Bool -> Rose GameState -> Int -> Int -> Int
miniMaxAB currentPlayer maxTurn tree alpha beta = case tree of
  Node gameState [] -> stateEvaluate gameState currentPlayer
  Node _ children
    |maxTurn -> maxValue children (-9999) alpha
    |otherwise -> minValue children 9999 beta
  where
    -- | Max turn. Update the beta value.
    maxValue :: [Rose GameState] -> Int -> Int -> Int
    maxValue gameTrees valueSoFar alphaSoFar = case gameTrees of
      [] -> valueSoFar
      x:xs
        |newAlpha >= beta -> newMaxValue
        |otherwise 
        -> maxValue xs newMaxValue newAlpha
        where
          childScore :: Rose GameState -> Int
          childScore child = miniMaxAB currentPlayer False child alphaSoFar beta
          newMaxValue = max (childScore x) valueSoFar
          newAlpha = max alpha newMaxValue

    -- | Min turn. Update the beta value.
    minValue :: [Rose GameState] -> Int -> Int -> Int
    minValue gameTrees valueSoFar betaSoFar = case gameTrees of
      [] -> valueSoFar
      x:xs
        |newBeta <= alpha -> newMinValue
        |otherwise 
        -> minValue xs newMinValue newBeta
        where
          childScore :: Rose GameState -> Int
          childScore child = miniMaxAB currentPlayer True child alpha betaSoFar
          newMinValue = min (childScore x) valueSoFar
          newBeta = min beta newMinValue


-- | This function evaluates a future game state for easyMode,
-- given the game state and the ai's player name.
stateEvaluatePlus :: GameState -> Player -> Int
stateEvaluatePlus state currentPlayer
  |getTurn state == GameOver (Winner currentPlayer) = 5000
  |getTurn state == GameOver (Winner opponent) = - 5000
  |stage <= 20 = 
    4 * positionScore scoreMatrix1 state currentPlayer +
    stabilityScore state currentPlayer -
    2 * mobilityScore state opponent
  |stage <= 40 =
    2 * positionScore scoreMatrix1 state currentPlayer +
    stabilityScore state currentPlayer -
    4 * mobilityScore state opponent
  |otherwise = 6 * positionScore scoreMatrix2 state currentPlayer +
    stabilityScore state currentPlayer
    where
      stage = totalPieces state - 5
      opponent = otherPlayer currentPlayer

-- | This function evaluates a game state for default.
-- given the game state and the ai's player name.
stateEvaluate :: GameState -> Player -> Int
stateEvaluate state currentPlayer
  |getTurn state == GameOver (Winner currentPlayer) = 5000
  |getTurn state == GameOver (Winner opponent) = - 5000
  |stage <= 20 = 
    4 * positionScore scoreMatrix1 state currentPlayer +
    stabilityScore state currentPlayer
  |stage <= 50 =
    2 * positionScore scoreMatrix1 state currentPlayer +
    stabilityScore state currentPlayer
  |otherwise = 2 * positionScore scoreMatrix2 state currentPlayer +
    stabilityScore state currentPlayer
    where
      stage = totalPieces state - 5
      opponent = otherPlayer currentPlayer

-- | This function evaluates a gamestate of a given player,
-- according a position weight matrix.
positionScore :: [Int] -> GameState -> Player -> Int
positionScore matrix (GameState _ _ board) currentPlayer = 
  sum (zipWith rankRule matrix (concat board))
  where
    rankRule :: Int -> Maybe Player -> Int
    rankRule score player
      |player == Just currentPlayer = score
      |player == Nothing = 0
      |otherwise = - score

-- | A score matrix for stage <= 40
scoreMatrix1 :: [Int]
scoreMatrix1 = 
  [200,-20, 20, 12, 12, 20,-20,200
  ,-20,-40,  2,  4,  4,  2,-40,-20
  , 20,  2, 10,  8,  8, 10,  2, 20
  , 12,  4,  8,  4,  4,  8,  4, 12
  , 12,  4,  8,  4,  4,  8,  4, 12
  , 20,  2, 10,  8,  8, 10,  2, 20
  ,-20,-40,  2,  4,  4,  2,-40,-20
  ,200,-20, 20, 12, 12, 20,-20,200]

-- | A score matrix for stage >= 40
scoreMatrix2 :: [Int]
scoreMatrix2 = 
  [16,14,14,14,14,14,14,16
  ,14,14,14,14,14,14,14,14
  ,14,14,14,14,14,14,14,14
  ,14,14,14,14,14,14,14,14
  ,14,14,14,14,14,14,14,14
  ,14,14,14,14,14,14,14,14
  ,14,14,14,14,14,14,14,14
  ,16,14,14,14,14,14,14,16]


-- | This function evaluates a gamestate of a given player, 
-- according mobility.
mobilityScore :: GameState -> Player -> Int
mobilityScore (GameState bound _ board) player = 
  4 * length (legalMoves (GameState bound (Turn player) board))

-- | This function evaluates a gamestate, according to the 
-- stabilities of the pieces of a given player.
stabilityScore :: GameState -> Player -> Int
stabilityScore (GameState _ _ b) currentPlayer = 
  corner1Score (0,0) 0 + corner2Score (0,7) 0 +
  corner3Score (7,0) 0 + corner4Score (7,7) 0
  where
  player = Just currentPlayer
  corner1Score :: (Int, Int) -> Int -> Int
  corner1Score (rowNum, colNum) scoreSoFar
    |rowNum == 7 || colNum == 7 = scoreSoFar
    |head (b !! rowNum) == player && head b !! colNum == player = 
          corner1Score (rowNum + 1, colNum + 1) (scoreSoFar + 20 * (rowNum + 1))
    |head (b !! rowNum) == player =
        corner1Score (rowNum + 1, colNum) (scoreSoFar + 10 * (rowNum + 1))
    |head b !! colNum == player = 
          corner1Score (rowNum, colNum + 1) (scoreSoFar + 10 * (colNum + 1))
    |otherwise = scoreSoFar

  corner2Score :: (Int, Int) -> Int -> Int
  corner2Score (rowNum, colNum) scoreSoFar
    |rowNum == 7 || colNum == 0 = scoreSoFar
    |last (b !! rowNum) == player && head b !! colNum == player = 
          corner2Score (rowNum + 1, colNum - 1) (scoreSoFar + 20 * (rowNum + 1))
    |last (b !! rowNum) == player =
          corner2Score (rowNum + 1, colNum) (scoreSoFar + 10 * (rowNum + 1))
    |head b !! colNum == player = 
          corner2Score (rowNum, colNum - 1) (scoreSoFar + 10 * (8 - colNum))
    |otherwise = scoreSoFar

  corner3Score :: (Int, Int) -> Int-> Int
  corner3Score (rowNum, colNum) scoreSoFar
    |rowNum == 0 || colNum == 7 = scoreSoFar
    |head (b !! rowNum) == player && last b !! colNum == player = 
          corner3Score (rowNum - 1, colNum + 1) (scoreSoFar + 20 * (colNum + 1))
    |head (b !! rowNum) == player =
          corner3Score (rowNum - 1, colNum) (scoreSoFar + 10 * (8 - rowNum))
    |last b !! colNum == player = 
          corner3Score (rowNum, colNum + 1) (scoreSoFar + 10 * (colNum + 1))
    |otherwise = scoreSoFar

  corner4Score :: 
    (Int, Int) -> Int-> Int
  corner4Score (rowNum, colNum) scoreSoFar
    |rowNum == 0 || colNum == 0 = scoreSoFar
    |last (b !! rowNum) == player && last b !! colNum == player = 
          corner4Score (rowNum - 1, colNum - 1) (scoreSoFar + 20 * (8 - rowNum))
    |last (b !! rowNum) == player =
          corner4Score (rowNum - 1, colNum) (scoreSoFar + 10 * (8 - rowNum))
    |last b !! colNum == player = 
          corner4Score (rowNum, colNum - 1) (scoreSoFar + 10 * (8 - colNum))
    |otherwise = scoreSoFar

-- ========================================================================== --
-- | The following are some helper functions.

-- | This function counts the total number of pieces of the board of a state.
totalPieces :: GameState -> Int
totalPieces (GameState _ _ board) = 
  length [p | Just p <- concat board, p == Player1 || p == Player2]

-- This function takes an unfinished game state and returns the current player.
-- If finished game state is unexpectely input, return Player 1.
getPlayer :: GameState -> Player
getPlayer state = case state of
  (GameState _ (Turn player) _) -> player
  _ -> Player1

-- | This function simluates the new state after a move.
-- | If the move is illegal, returns the same state.
implementMove :: GameState -> Move -> GameState
implementMove state move = case applyMove state move of
  Just newState -> newState
  Nothing -> state

-- | Switch the player of an unfinished state.
-- If a finished state is unexpectedly input, then it will do nothing.
switchPlayer :: GameState -> GameState
switchPlayer st = case st of
  GameState bound (Turn player) board -> 
    GameState bound (Turn (otherPlayer player)) board
  _ -> st


-- ========================================================================== --

-- Reference Code

-- Visualises a rose tree.
instance Show a => Show (Rose a) where
  show = unlines . layout
    where
      layout :: Show a => Rose a -> [String]
      layout (Node v []) = [show v]
      layout (Node v children) = 
        [show v] ++ concat (map indent (map layout children))
      indent :: [String] -> [String]
      indent = map ("  "++)

-- Hosking, A 2016 "Nim", <https://cs.anu.edu.au/courses/comp1100/lectures/10/>

-- ========================================================================== --
