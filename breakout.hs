{- 

pong - a very simple FunGEn example.
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2001  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Main where
    import Graphics.UI.Fungen
    import Graphics.Rendering.OpenGL (GLdouble)
    
    data GameAttribute = Score Int
     
    width = 800
    height = 800
    w = fromIntegral width :: GLdouble
    h = fromIntegral height :: GLdouble
    
    main :: IO ()
    main = do
      let winConfig = ((100,80),(width,height),"Breakout 2.0")
          bmpList = [("ivan.bmp", Nothing)]
          gameMap = textureMap 0 30 30 w h
          bar     = objectGroup "barGroup"  [createBar]
          ball    = objectGroup "ballGroup" [createBall]
          block   = objectGroup "blockGroup" [createBlock]
          initScore = Score 0
          input = [
            (SpecialKey KeyRight, StillDown, moveBarToRight)
            ,(SpecialKey KeyLeft,  StillDown, moveBarToLeft)
            ,(Char 'q',            Press,     \_ _ -> funExit)
            ]
      funInit winConfig gameMap [bar,ball,block] () initScore input gameCycle (Timer 30) bmpList
    
    createBall :: GameObject ()
    createBall =
      let ballPic = Basic (Circle 10.0 1.0 1.0 1.0 Filled)
      in object "ball" ballPic False (w/2,h/2) (-8,8) ()
    
    createBlock :: GameObject()
    createBlock =
      let blockBound = [(0,0),(0,30),(100,30),(100,0)]
          blockPic = Basic (Polyg blockBound 0 0 1.0 Filled)
      in object "block" blockPic False (500,500) (0,0) ()


    createBar :: GameObject ()
    createBar =
      let barBound = [(0,0),(0,15),(80,15),(80,0)]
          barPic   = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
      in object "bar" barPic False (w/2,30) (0,0) ()
    
    moveBarToRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    moveBarToRight _ _ = do
      obj     <- findObject "bar" "barGroup"
      (pX,pY) <- getObjectPosition obj
      (sX,_)  <- getObjectSize obj
      if (pX + (sX/2) + 5 <= w)
       then (setObjectPosition ((pX + 20),pY) obj)
       else (setObjectPosition ((w - (sX/2)),pY) obj)
    
    moveBarToLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    moveBarToLeft _ _ = do
      obj <- findObject "bar" "barGroup"
      (pX,pY) <- getObjectPosition obj
      (sX,_)  <- getObjectSize obj
      if (pX - (sX/2) - 5 >= 0)
        then (setObjectPosition ((pX - 20),pY) obj)
        else (setObjectPosition (sX/2,pY) obj)
    
    gameCycle :: IOGame GameAttribute () () () ()
    gameCycle = do
      (Score n) <- getGameAttribute
      printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0
      block <- findObject "block" "blockGroup"
      ball <- findObject "ball" "ballGroup"
      col1 <- objectLeftMapCollision ball
      col2 <- objectRightMapCollision ball
      when (col1 || col2) (reverseXSpeed ball)
      col3 <- objectTopMapCollision ball
      when col3 (reverseYSpeed ball)
      col4 <- objectBottomMapCollision ball
      when col4 $ do
        -- funExit
        setGameAttribute (Score 0)
        reverseYSpeed ball
    
      bar <- findObject "bar" "barGroup"
      col5 <- objectsCollision ball bar
      let (_,vy) = getGameObjectSpeed ball
      when (and [col5, vy < 0])  (do reverseYSpeed ball
                                     setGameAttribute (Score (n + 10)))
      showFPS TimesRoman24 (w-40,0) 1.0 0.0 0.0
    
    