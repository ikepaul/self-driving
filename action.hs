module Action where

data Action = CarForward | CarRight | CarBackwards | CarLeft deriving (Eq, Show)

readAction :: Int -> Maybe Action
readAction key = case key of
  38 -> Just CarForward -- up arrow
  39 -> Just CarRight -- right arrow
  40 -> Just CarBackwards -- down arrow
  37 -> Just CarLeft -- left arrow
  _ -> Nothing

data Controller = Controller {carForward :: Bool, carRight :: Bool, carBackwards :: Bool, carLeft :: Bool}

newController :: Controller
newController = Controller False False False False

toggleController :: Action -> Bool -> Controller -> Controller
toggleController CarForward b (Controller _ rt re lt) = Controller b rt re lt
toggleController CarRight b (Controller fd _ re lt) = Controller fd b re lt
toggleController CarBackwards b (Controller fd rt _ lt) = Controller fd rt b lt
toggleController CarLeft b (Controller fd rt re _) = Controller fd rt re b

--toggleController a b [] = []
--toggleController a b ((ca, cb) : cs) = if ca == a then (ca, b) : cs else (ca, cb) : toggleController a b cs
