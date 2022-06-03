import Data.List
import System.IO 
import System.Random
import Text.Read
import qualified Data.Text.IO 
import Data.Char 
import System.Environment
import qualified Data.Map 

data Cards = A | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K deriving (Show, Enum, Bounded, Read) 

main = do
  putStrLn "---------------------------------------"
  putStrLn "This is Blackjack, let's start to play!"
  newGame

-- Simply starts a new game
newGame :: IO()
newGame = do
  dealerGen <- newStdGen
  playerGen <- newStdGen
  game (drawDealerCards dealerGen 0) (drawPlayerCards playerGen 2) 0

-- Core
game :: [Cards] -> [Cards] -> Double -> IO()
game dealerCards playerCards bet = do
  if bet == 0
    then do
      putStrLn "Enter y if you want to play a game, enter anything else if you want to exit\n"
      startPlay <- getLine
      if map toLower startPlay == "y"
        then do
          putStrLn "\nLet's play a new game!:\n"
          putStrLn "Give your bet:\n"
          betValue <- getLine
          if (read betValue::Double) < 1
            then do 
              putStrLn "Enter atleast 1\n"
              game dealerCards playerCards 0
            else do 
              putStrLn "Lets start!\n"
              game dealerCards playerCards (read betValue::Double)
        else do
          putStrLn "\nBye Bye!\n"
  
    -- Blackjack case
  else if (sumOfCards $ handValue playerCards) == 21
    then do 
      putStrLn $ "Dealer's cards: " ++ showCards dealerCards ++ "\n"
      putStrLn $ "Your cards: " ++ showCards playerCards ++ "\n"
      -- Win 1.5x your bet + regular winning
      putStrLn $ "You got a blackjack! You won: " ++ show (((bet::Double) / 2) + ((bet::Double) * 2)) ++ "\n"
      newGame

  -- If dealer's cards > 21 -> player wins
  else if (sumOfCards $ handValue dealerCards) > 21
    then do 
      putStrLn $ "Dealer's cards: " ++ showCards dealerCards ++ "\n"
      putStrLn $ "Your cards: " ++ showCards playerCards ++ "\n"
      putStrLn $ "Dealer got above 21! You won: " ++ show ((bet::Double) * 2) ++ "\n"
      newGame

  -- Vice versa
  else if (sumOfCards $ handValue playerCards) > 21
    then do 
      putStrLn $ "Dealer's cards: " ++ showCards dealerCards ++ "\n"
      putStrLn $ "Your cards: " ++ showCards playerCards ++ "\n"
      putStrLn $ "Dealer wins this wound. You are above 21. You lost: " ++ show (bet::Double) ++ "\n"
      newGame
  
  -- Push case
  else if (sumOfCards $ handValue playerCards) == (sumOfCards $ handValue dealerCards)
    then do 
      putStrLn $ "Dealer's cards: " ++ showCards dealerCards ++ "\n"
      putStrLn $ "Your cards: " ++ showCards playerCards ++ "\n"
      putStrLn "It is a push, take your money back!\n"
      newGame

  -- Here is the beef -> do player want to hit or stand?
  else do 
    putStrLn $ "Dealer's cards: " ++ showCards dealerCards ++ "\n"
    putStrLn $ "Your cards: " ++ showCards playerCards ++ "\n"
    putStrLn "Type h for HIT \nType s for STAND"
    command <- getLine
    if map toLower command == "h"
      then do 
        playerGen <- newStdGen
        let drawCard = drawPlayerCard playerGen
        let newPlayerCards = playerCards ++ drawCard
        game dealerCards newPlayerCards bet
    else if map toLower command == "s"
      then if (compareHands dealerCards playerCards) == "Player Win"
        then do 
          putStrLn $ "Dealer's cards: " ++ showCards dealerCards ++ "\n"
          putStrLn $ "Your cards: " ++ showCards playerCards ++ "\n"
          putStrLn $ "You are closer to 21 than the dealer! You Win: " ++ show ((bet::Double) * 2) ++ "\n"
          newGame
      else if (compareHands dealerCards playerCards) == "Player Lose"
        then do 
          putStrLn $ "Dealer's cards: " ++ showCards dealerCards ++ "\n"
          putStrLn $ "Your cards: " ++ showCards playerCards ++ "\n"
          putStrLn $ "The dealer beat you this time! You Lost: " ++ show (bet::Double) ++ "\n"
          newGame              
      else do 
        putStrLn $ "Dealer's cards: " ++ showCards dealerCards ++ "\n"
        putStrLn $ "Your cards: " ++ showCards playerCards ++ "\n"
        putStrLn "It is a push, take your money back!\n"
        newGame 
    else do 
      game dealerCards playerCards bet



-- Draws player cards initially
drawPlayerCards :: StdGen -> Int -> [Cards]
drawPlayerCards gen stop
  |stop /= 0 = toEnum (randomNumber - 1) : drawPlayerCards newGen (stop - 1)
  |otherwise = []
  where (randomNumber, newGen) = randomR(1,13) gen :: (Int,StdGen)

-- Draws dealer cards
drawDealerCards :: StdGen -> Int -> [Cards]
drawDealerCards gen total
  |total < 11 && randomNumber == 1 = toEnum (randomNumber - 1) : drawDealerCards newGen (total + randomNumber + 10)
  |total < 17 = toEnum (randomNumber - 1) : drawDealerCards newGen (total + randomNumber)
  |otherwise = []
  where (randomNumber, newGen) = randomR(1,13) gen :: (Int, StdGen)

-- Draws one player card
drawPlayerCard :: StdGen -> [Cards]
drawPlayerCard gen = [toEnum (randomNumber - 1)]
  where (randomNumber, newGen) = randomR(1,13) gen :: (Int,StdGen)

-- Hand value
handValue :: [Cards] -> [Int]
handValue [] = []
handValue (card:cx)
  |fromEnum card >= 10 = fromEnum 10 : handValue cx
  |otherwise = (fromEnum (card) + 1) : handValue cx

-- Represent cards
showCards :: [Cards] -> String 
showCards  [] = ""
showCards (card:cs)
  |fromEnum card < 10 && fromEnum card > 0 = show (fromEnum (card) + 1) ++ " " ++ showCards cs
  |otherwise = show card ++ " " ++ showCards cs

-- Gets sum for the cards
sumOfCards :: [Int] -> Int
sumOfCards cards
  |sum cards < 12 && 1 `elem` cards = sum (cards) + 1
  |otherwise = sum cards

-- Compares dealers and players hands
compareHands :: [Cards] -> [Cards] -> String
compareHands dealerCards playerCards
  |sumOfCards (handValue dealerCards) == sumOfCards (handValue playerCards) = "Push"
  |sumOfCards (handValue dealerCards) > sumOfCards (handValue playerCards) = "Player Lose"
  |sumOfCards (handValue dealerCards) < sumOfCards (handValue playerCards) = "Player Win"