{-|
Module      : AITests
Description : Tests for your AI functions
Copyright   : (c) 2021 Haoting Chen
License     : MIT
-}
module AITests where

import           AI
import           Othello
import           Testing
import           OthelloTests

aiTests :: Test
aiTests = TestGroup "AI"
  [
   easyModeTest
  ,hardModeTest
  ,bestMoveTest
  ,othelloTreeTest
  ,filterMovesTest
  ,maxScoreMoveTest
  ,miniMaxABTest
  ,stateEvaluateTest
  ,stateEvaluatePlusTest
  ,positionScoreTest
  ,mobilityScoreTest
  ,stabilityScoreTest
  ,totalPiecesTest
  ,switchPlayerTest
  ,implementMoveTest
  ]


-- | Test whether the ai easyMode can choose some obviously best moves.
easyModeTest :: Test
easyModeTest = TestGroup "easyMove"
  [ Test "Corner capture (0,7)"
    (assertEqual (easyMode testState4) 
    (Move (0,7))),
    Test "Corner capture (7,7)"
    (assertEqual (easyMode testState5) 
    (Move (7,7))),
    Test "only one move"
    (assertEqual (easyMode testState6) 
    (Move (0,7)))
  ]

-- | Test whether the ai default can choose some obviously best moves.
hardModeTest :: Test
hardModeTest = TestGroup "default"
  [ Test "Corner capture (0,7)"
    (assertEqual (hardMode testState4 2) 
    (Move (0,7))),
    Test "Corner capture (7,7)"
    (assertEqual (hardMode testState5 2) 
    (Move (7,7))),
    Test "only one move"
    (assertEqual (hardMode testState6 2) 
    (Move (0,7)))
  ]


-- Test if the generated othello tree has a correct depth and leaves number.
-- 
-- If the expected player name of the root does not agree with the player name
-- recored in the root of the game state (Skip turn case), the function should 
-- add a extra node with the correct player name above the previous root.

othelloTreeTest :: Test
othelloTreeTest = TestGroup "othelloTree"
  [ Test "correct depth 1"
    (assertEqual (roseDepth (othelloTree 5 Player2 testState1)) 5),
    Test "correct depth 2"
    (assertEqual (roseDepth (othelloTree 6 Player1 testState3')) 1),
    Test "correct leaves number"
    (assertEqual (length (roseLeaves (othelloTree 2 Player2 testState1)))
    (length (legalMoves testState1))),
    Test "unexpected player of the root"
    (assertEqual (roseSize (othelloTree 5 Player1 testState1))
    (1 + roseSize (othelloTree 4 Player2 testState1)))
  ]

-- | Test if the function correctly filters some moves.
filterMovesTest :: Test 
filterMovesTest = TestGroup "filterMoves"
  [ Test "all bad moves"
    (assertEqual (filterMoves testState9
    [m11,m61,m66,m16]) 
    [m11,m61,m66,m16]),
    Test "any bad move"
    (assertEqual (filterMoves testState9
    [m11,m61,m66,m16,m74])
    [m74]),
    Test "any good move"
    (assertEqual (filterMoves testState9
    [m74,m03,m00,m77]) 
    [m00,m77]),
    Test "no good or bad moves 1"
    (assertEqual (filterMoves testState9
    [m74,m03])
    [m74,m03]),
    Test "no good or bad moves 2"
    (assertEqual (filterMoves testState10
    [m11,m61,m66,m16])
    [m11,m61,m66,m16])
  ]
  where
    -- bad moves
    m11 = Move (1,1)
    m61 = Move (6,1)
    m66 = Move (6,6)
    m16 = Move (1,6)
    -- good moves
    m00 = Move (0,0)
    m77 = Move (7,7)
    -- other moves
    m74 = Move (7,4)
    m03 = Move (0,3)


-- | Test whether the function can choose some obviously best move.
bestMoveTest :: Test
bestMoveTest = TestGroup "bestMove"
  [ Test "look ahead (Corner capture (0,7))"
    (assertEqual (bestMove (legalMoves testState4) testState4 True) 
    (Move (0,7))),
    Test "Without looking ahead (Corner capture (0,7))"
    (assertEqual (bestMove (legalMoves testState4) testState4 False) 
    (Move (0,7))),
    Test "look ahead (Corner capture (7,7))"
    (assertEqual (bestMove (legalMoves testState5) testState5 True) 
    (Move (7,7))),
    Test "Without looking ahead (Corner capture (7,7))"
    (assertEqual (bestMove (legalMoves testState5) testState5 False) 
    (Move (7,7))),
    Test "look ahead (only one move)"
    (assertEqual (bestMove (legalMoves testState6) testState6 True) 
    (Move (0,7))),
    Test "Without looking ahead (only one move)"
    (assertEqual (bestMove (legalMoves testState6) testState6 False) 
    (Move (0,7)))
  ]

-- | Test if the function can correctly choose the move with the high score.
maxScoreMoveTest :: Test
maxScoreMoveTest = TestGroup "maxScoreMove"
  [ Test "only one move"
    (assertEqual (maxScoreMove [move1]) (Move (1,1))),
    Test "same score"
    (assertEqual (maxScoreMove [move3,move1,move5]) (Move (5,6))),
    Test "some moves"
    (assertEqual (maxScoreMove [move1,move2,move3,move4,move5]) (Move (7,7)))
  ]
  where
    move1 = (Move (1,1), -100 :: Int)
    move2 = (Move (4,3), 10 :: Int)
    move3 = (Move (5,6), 30 :: Int)
    move4 = (Move (7,7), 200 :: Int)
    move5 = (Move (3,3), 30 :: Int)

-- | Test if the score of a state evaluated by Minimax Alpha Beta is same as the
-- score evaluated by normal Minimax.
miniMaxABTest :: Test
miniMaxABTest = TestGroup "miniMaxABTest"
  [ Test "a Player2 turn's state, Player2 is max." (assertEqual
    (miniMax Player2 True (othelloTree 5 Player2 testState1))
    (miniMaxAB Player2 True (othelloTree 5 Player2 testState1) (-9999) 9999)),
    Test "a Player2 turn's state, Player2 is min." (assertEqual
    (miniMax Player2 False (othelloTree 5 Player2 testState1))
    (miniMaxAB Player2 False (othelloTree 5 Player2 testState1) (-9999) 9999)),
    Test "a Player1 turn's state, Player1 is max" (assertEqual
    (miniMax Player2 True (othelloTree 5 Player2 testState3))
    (miniMaxAB Player2 True (othelloTree 5 Player2 testState3) (-9999) 9999)),
    Test "a Player1 turn's state, Player1 is min" (assertEqual
    (miniMax Player2 False (othelloTree 5 Player2 testState3))
    (miniMaxAB Player2 False (othelloTree 5 Player2 testState3) (-9999) 9999))
  ]

-- | Test if the overall score of a player in a state calculated by the 
-- function is same as the one that is calculated by positionScore and
-- mobilityScore first, and then multiplied and summed by hand.
stateEvaluateTest :: Test 
stateEvaluateTest = TestGroup "stateEvaluate"
  [Test "the overall score of Player1 in testState7" 
  (assertEqual (stateEvaluate testState7 Player1) 1226),
  Test "the overall score of Player2 in testState7" 
  (assertEqual (stateEvaluate testState7 Player2) (-816)),
  Test "the overall score of Player1 in testState8" 
  (assertEqual (stateEvaluate testState8 Player1) (-496)),
  Test "the overall score of Player2 in testState8" 
  (assertEqual (stateEvaluate testState8 Player2) 1056)
  ]

-- | Test if the overall score of a player in a state calculated by the 
-- function is same as the one that is calculated by positionScore, 
-- mobilityScore, and stabilityScore first, and multiplied and summed by hand.
stateEvaluatePlusTest :: Test 
stateEvaluatePlusTest = TestGroup "stateEvaluatePlus"
  [Test "the overall score of Player1 in testState7" 
  (assertEqual (stateEvaluatePlus testState7 Player1) 1190),
  Test "the overall score of Player2 in testState7" 
  (assertEqual (stateEvaluatePlus testState7 Player2) (-780)),
  Test "the overall score of Player1 in testState8" 
  (assertEqual (stateEvaluatePlus testState8 Player1) (-672)),
  Test "the overall score of Player2 in testState8" 
  (assertEqual (stateEvaluatePlus testState8 Player2) 1040)
  ]


-- | Test if the position score of a player in a state calculated by the 
-- function is same as the one calculated by hand.
positionScoreTest :: Test 
positionScoreTest = TestGroup "positionScore"
  [Test "the positional score of Player1 in testState7 according scoreMatrix1" 
  (assertEqual (positionScore scoreMatrix1 testState7 Player1) 408),
  Test "the positional score of Player2 in testState7 according scoreMatrix1" 
  (assertEqual (positionScore scoreMatrix1 testState7 Player2) (-408)),
  Test "the positional score of Player1 in testState7 according scoreMatrix2" 
  (assertEqual (positionScore scoreMatrix2 testState7 Player1) 130),
  Test "the positional score of Player2 in testState7 according scoreMatrix2" 
  (assertEqual (positionScore scoreMatrix2 testState7 Player2) (-130)),

  Test "the positional score of Player1 in testState8 according scoreMatrix1" 
  (assertEqual (positionScore scoreMatrix1 testState8 Player1) (-248)),
  Test "the positional score of Player2 in testState8 according scoreMatrix1" 
  (assertEqual (positionScore scoreMatrix1 testState8 Player2) 248),
  Test "the positional score of Player1 in testState8 according scoreMatrix2" 
  (assertEqual (positionScore scoreMatrix2 testState8 Player1) (-226)),
  Test "the positional score of Player2 in testState8 according scoreMatrix2" 
  (assertEqual (positionScore scoreMatrix2 testState8 Player2) 226)
  ]

-- | Test if the mobility score of a player in a state calculated by the 
-- function is same as the one calculated by hand.
mobilityScoreTest :: Test 
mobilityScoreTest = TestGroup "mobilityScore"
  [Test "the mobility score of Player1 in testState7" 
  (assertEqual (mobilityScore testState7 Player1) 36),
  Test "the mobility score of Player2 in testState7" 
  (assertEqual (mobilityScore testState7 Player2) 20),
  Test "the mobility score of Player1 in testState8" 
  (assertEqual (mobilityScore testState8 Player1) 4),
  Test "the mobility score of Player2 in testState8" 
  (assertEqual (mobilityScore testState8 Player2) 44)
  ]

-- | Test if the stability score of a player in a state calculated by the 
-- function is same as the one calculated by hand.
stabilityScoreTest :: Test 
stabilityScoreTest = TestGroup "stabilityScore"
  [Test "the stability score of Player1 in testState7" 
  (assertEqual (stabilityScore testState7 Player1) 410),
  Test "the stability score of Player2 in testState7" 
  (assertEqual (stabilityScore testState7 Player2) 0),
  Test "the stability score of Player1 in testState8" 
  (assertEqual (stabilityScore testState8 Player1) 0),
  Test "the stability score of Player2 in testState8" 
  (assertEqual (stabilityScore testState8 Player2) 560)
  ]


-- | Test if the total pieces number in a state calculated by the 
-- function is same as the number calculated by hand.
totalPiecesTest :: Test 
totalPiecesTest = TestGroup "totalPieces"
  [Test "the total pieces in testState1" 
  (assertEqual (totalPieces testState1) 8),
  Test "the total pieces in testState3" 
  (assertEqual (totalPieces testState3) 14),
  Test "the total pieces in testState7" 
  (assertEqual (totalPieces testState7) 53),
  Test "the total pieces in testState8" 
  (assertEqual (totalPieces testState8) 40)
  ]

-- | Test if the function get the correct player name from a state.
getPlayerTest :: Test 
getPlayerTest = TestGroup "getPlayer"
  [Test "an unfinished state of Player1" 
  (assertEqual (getPlayer testState7) Player1),
  Test "an unfinished state of Player2" 
  (assertEqual (getPlayer testState8) Player2),
  Test "other input" 
  (assertEqual (getPlayer testState2) Player1)
  ]

-- | Test if the function correctly implement a move.
implementMoveTest :: Test
implementMoveTest = TestGroup "implementMove"
  [Test "a legal move" 
  (assertEqual (implementMove testState3 (Move (3,3))) testState3'),
  Test "a illegal move" 
  (assertEqual (implementMove testState3 (Move (6,6))) testState3)
  ]

--- | Test if the function correctly switch a play.
switchPlayerTest :: Test 
switchPlayerTest = TestGroup "switchPlayer"
  [Test "an unfinished game state" 
  (assertEqual (switchPlayer oddState) newState),
  Test "a finished game state"
  (assertEqual (switchPlayer testState2) testState2)
  ]
  where
    oddState = GameState (8,8) (Turn Player1) (initialBoard (8,8))
    newState = GameState (8,8) (Turn Player2) (initialBoard (8,8))


-- ========================================================================== --

-- | The following are the helper functions for testing

-- | Calculate the size of a rose tree
roseSize :: Rose a -> Int
roseSize (Node _ roseNodes) = foldr (\r x -> roseSize r + x) 1 roseNodes

-- | Calculate the depth of a rose tree
roseDepth :: Rose a -> Int
roseDepth tree = case tree of
    Node _ [] -> 1
    Node _ children -> 1 + maximum (map roseDepth children)

-- | Extract the leaves of a rose tree as a list
roseLeaves :: Rose a -> [a]
roseLeaves tree = case tree of
    Node a [] -> [a]
    Node _ roseNodes -> concatMap roseLeaves roseNodes

-- | In this state ,there is a chance for Player1 to capture the corner (0,7).
testState4 :: GameState
testState4 = GameState (8,8) (Turn Player2) board
    where
        board = readBoard boardString
        boardString = 
            ["......O."
            ,"..XXXO.."
            ,".OOXXO.."
            ,".OOXX..."
            ,"XXXXX..."
            ,"XXXX.X.."
            ,"XXXX...."
            ,"..O....."]

-- | In this state ,there is a chance for Player1 to capture the corner (7,7).
testState5 :: GameState
testState5 = GameState (8,8) (Turn Player1) board
    where
        board = readBoard boardString
        boardString = 
            ["........"
            ,"........"
            ,"......X."
            ,"..XXXX.."
            ,"..OXO..."
            ,"...OXO.."
            ,"..X.O.O."
            ,"........"]

-- | In this state ,(0,7) is the only move that Player 1 can choose.
testState6 :: GameState
testState6 = GameState (8,8) (Turn Player1) board
    where
        board = readBoard boardString
        boardString = 
            ["XXXXXXXX"
            ,"XXXXOXXX"
            ,"XXXOXOXX"
            ,"XXOXOOXX"
            ,"XOXOXOXX"
            ,"XOOOOXXX"
            ,"XOOXXXXX"
            ,".OOXXXXX"]

-- | A state that Player 1 has a high mobility, stability and positional score.
testState7 :: GameState
testState7 = GameState (8,8) (Turn Player1) board
    where
        board = readBoard boardString
        boardString = 
            ["XXXXO..."
            ,"XXXXXO.O"
            ,"XXXOOOO."
            ,"XXOXOOOO"
            ,"XOXOXXXX"
            ,"OOOOOXX."
            ,"..XOXXO."
            ,"..OXXXXX"]

-- | A state that Player 2 has a high mobility, stability and positional score.
testState8 :: GameState
testState8 = GameState (8,8) (Turn Player1) board
    where
        board = readBoard boardString
        boardString = 
            ["........"
            ,"O...X..."
            ,"OOXXX..."
            ,"OOXXXX.."
            ,"OOXOX..."
            ,"OOOOOXOO"
            ,"OOOOOOX."
            ,"OOOOOOO."]

-- | A state that contains some bad moves.
testState9 :: GameState
testState9 = GameState (8,8) (Turn Player1) board
    where
        board = readBoard boardString
        boardString = 
            ["..O..O.."
            ,"..OXXO.."
            ,"OOOXXOOO"
            ,".XXXXXXO"
            ,"OXXXXXX."
            ,"OOOXXOOO"
            ,"..OXXO.."
            ,"..O..O.."]

-- | A state that does not contains bad moves.
testState10 :: GameState
testState10 = GameState (8,8) (Turn Player1) board
    where
        board = readBoard boardString
        boardString = 
            ["X.O..O.X"
            ,"..OXXO.."
            ,"OOOXXOOO"
            ,".XXXXXXO"
            ,"OXXXXXX."
            ,"OOOXXOOO"
            ,"..OXXO.."
            ,"X.O..O.X"]

