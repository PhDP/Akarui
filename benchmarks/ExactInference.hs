import Data.Maybe (fromMaybe)
import qualified Manticore.MarkovLogic as ML

mln = ML.fromStrings
        [ "∀x Smoking(x) ⇒ Cancer(x) 1.5"
        , "∀xy Friend(x, y) ∧ Smoking(x) ⇒ Smoking(y) 1.1"
        , "∀x,y Friend(x, y) ⇔ Friend(y, x) 2.0"]

cs = ["Jerry", "Elaine", "George"]

ask = ML.ask mln cs

p0 = fromMaybe (-10.0) $ ask "P(Cancer(Jerry) | Smoking(George))"
p1 = fromMaybe (-10.0) $ ask "P(Cancer(Jerry) | Smoking(George), Friend(George, Jerry))"
p2 = fromMaybe (-10.0) $ ask "P(Cancer(Jerry) | Smoking(George), Friend(Jerry, George))"
mean = (p0 + p1 + p2) / 3.0

main :: IO ()
main = putStrLn $ show mean

