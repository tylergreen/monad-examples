import Control.Monad

-- Cloned Sheep example

fathers :: [(Sheep, Sheep)]
fathers = [("brewster", "elvis")]

mothers :: [(Sheep, Sheep)]
mothers = [("brewster", "mary")]

father :: Sheep -> Maybe Sheep
father s = lookup s fathers

mother :: Sheep -> Maybe Sheep
mother s = lookup s mothers

parent :: Sheep -> [ Sheep ]
parent s = mplus
           (maybeToList (mother s)) 
           (maybeToList (father s))

grandparent :: Sheep -> [ Sheep ]
grandparent s = do p <- parent s
                   parent p

traceFamily s l = foldM getParent s l 
                  where getParent s f = f s






                     