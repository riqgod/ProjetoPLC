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
    
    data GameAttribute = Score Int Int
    data TileAttribute = NoTileAttribute

    type Blocks = GameObject()
    type Ball = GameObject()
    type BreakoutAction a = IOGame GameAttribute () () TileAttribute a
    type BreakoutTile = Tile TileAttribute
    type BreakoutMap = TileMatrix TileAttribute

    rows = 4
    columns = 5
    boundsx = 30
    boundsy = 20
     
    width = 800
    height = 900
    w = fromIntegral width :: GLdouble
    h = fromIntegral height :: GLdouble
    
    magenta :: InvList
    magenta = Just [(255,0,255)]

    tileSize :: GLdouble
    tileSize = 30.0

    border1,free1,ball,field :: Int
    border1 = 1
    free1 = 2
    ball = 3
    field = 4

    main :: IO ()
    main = do
      let winConfig = ((100,80),(width,height),"Breakout 2.0")
          bmpList = [("oi.bmp",      Nothing), 
                     ("border1.bmp",   magenta),
                     ("free1.bmp",     magenta),
                     ("ball.bmp",     magenta),
                     ("field.bmp",   Nothing)]

          gameMap = multiMap [(tileMap map1 w h)] 0 

          bar     = objectGroup "barGroup"  [createBar]
          ball    = objectGroup "ballGroup" createBall
          blocks  = objectGroup "blockGroup" initBlocks

          initAttributes = (Score 0 (rows * ((columns*2)-1) ))
          input = [
            (SpecialKey KeyRight, StillDown, moveBarToRight)
            ,(SpecialKey KeyLeft,  StillDown, moveBarToLeft)
            ,(Char 'q',            Press,     \_ _ -> funExit)
            ]
      funInit winConfig gameMap [bar,ball,blocks] () initAttributes input gameCycle (Timer 8) bmpList
    
    createBlocks :: Int -> Int -> [Blocks]
    createBlocks row column
     | (row>= rows) = []
     | (column >= columns) = createBlocks (row+1) (-4)
     | otherwise = do
      let offsetx = boundsx*2+5
      let offsety = boundsy*2+5
      let x = (w/2)+(offsetx * (fromIntegral column))
      let y = (h - 300)+((fromIntegral row) * offsety)
      let name = "block" ++ (show row) ++ (show column)
      (createBlockAt (x, y) (name):(createBlocks (row) (column+1)) )

    createBlockAt :: (GLdouble, GLdouble) -> String -> Blocks
    createBlockAt (pX, pY) name =
       let blockBounds = [(-boundsx,-boundsy),(boundsx,-boundsy),(boundsx,boundsy),(-boundsx,boundsy)]
           blockPoly = Basic (Polyg blockBounds 0.0 0.0 1.0 Filled)
       in object name blockPoly False (pX,pY) (0,0) ()
  
    initBlocks :: [Blocks]
    initBlocks = (createBlocks 0 (-4) )

    createBall :: [GameObject ()]
    createBall =
      let ballPic = Tex (tileSize,tileSize) ball
      in [(object "ball1" ballPic False (400,100) (2,-2) ()),
          (object "ball2" ballPic False (400,100) (2,-2) ())]
    
    createBlock :: GameObject()
    createBlock =
      let blockBound = [(0,0),(0,30),(100,30),(100,0)]
          blockPic = Basic (Polyg blockBound 0 0 1.0 Filled)
      in object "block" blockPic True (500,500) (0,0) ()


    createBar :: GameObject ()
    createBar =
      let barBound = [(0,0),(0,15),(80,15),(80,0)]
          barPic   = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
      in object "bar" barPic False (w/2,30) (0,0) ()
    
    moveBarToRight :: Modifiers -> Position -> IOGame GameAttribute () () TileAttribute ()
    moveBarToRight _ _ = do
      obj     <- findObject "bar" "barGroup"
      (pX,pY) <- getObjectPosition obj
      (sX,_)  <- getObjectSize obj
      if (pX + (sX/2) + 5 <= w)
       then (setObjectPosition ((pX + 20),pY) obj)
       else (setObjectPosition ((w - (sX/2)),pY) obj)
    
    moveBarToLeft :: Modifiers -> Position -> IOGame GameAttribute () () TileAttribute ()
    moveBarToLeft _ _ = do
      obj <- findObject "bar" "barGroup"
      (pX,pY) <- getObjectPosition obj
      (sX,_)  <- getObjectSize obj
      if (pX - (sX/2) - 5 >= 0)
        then (setObjectPosition ((pX - 20),pY) obj)
        else (setObjectPosition (sX/2,pY) obj)
    

    checkBlockCollision :: Ball -> [Blocks] -> BreakoutAction()
    checkBlockCollision ball [] = do return()
    checkBlockCollision ball (x:xs) = do
      (bX,bY) <- getObjectPosition ball
      (pX,pY) <- getObjectPosition x
      (ballSizeX,ballSizeY) <- getObjectSize ball
      (sX,sY) <- getObjectSize x
      col <- objectsCollision ball x
      let (pSizeX,pSizeY) = (sX/2,sY/2)
      let (bSizeX,bSizeY) = (ballSizeX/2,ballSizeY/2)
      --liftIOtoIOGame(putStrLn(show (length xs)))               
      when(col) (do 
        setObjectAsleep True x
        checkBallPos ball (bX,bY) (pX,pY) (bSizeX,bSizeY) (pSizeX,pSizeY))
      checkBlockCollision ball xs
            
    -- ball -> block
    checkBallPos :: Ball -> (GLdouble, GLdouble) -> (GLdouble, GLdouble) -> (GLdouble,GLdouble) -> (GLdouble,GLdouble) -> BreakoutAction ()
    checkBallPos ball (bX, bY) (pX, pY) (bSizeX,bSizeY) (pSizeX,pSizeY) = do
      (Score n blocks) <- getGameAttribute
      (vx,vy) <- getObjectSpeed ball
      if((pY-bY)>=(pSizeY+bSizeY-vy))
        then( do
          reverseYSpeed ball
          liftIOtoIOGame(putStrLn( "primeiro if")))
        else if((bY-pY)>=(pSizeY+bSizeY+vy))
          then(do
            reverseYSpeed ball
            liftIOtoIOGame(putStrLn( "second if")))
          else(do
            reverseXSpeed ball
            liftIOtoIOGame(putStrLn( "else")))
      {-if( (Vx>0) && (Vy>0) )
        then()
        else if((Vx<0) && (Vy>0) )
          then()
          else if( (Vx>0) && (Vy<0) )
            then()
            else if( (Vx<0) && (Vy<0) )
              then()-}



      {-if((bX) == (pX+pSizeX+bSizeX)) || ((bX) == (pX-pSizeX-bSizeX) )
        then(do 
             reverseXSpeed ball
             setGameAttribute(Score (n+10) blocks ))
        else if( ((bY)==(pY+pSizeY+bSizeY)) || ((bY)==(pY-pSizeY-bSizeY)) )
          then(do 
               reverseYSpeed ball
               setGameAttribute(Score (n+10) blocks ))
          else (do
                setGameAttribute(Score n blocks ))-}
        
    checkBalls :: [Ball] -> BreakoutAction()
    checkBalls [] = return()
    checkBalls (x:xs) = do
      (Score n blocks) <- getGameAttribute
      flags <- getObjectsFromGroup "blockGroup"
      checkBlockCollision x flags
      col1 <- objectLeftMapCollision x
      col2 <- objectRightMapCollision x
      when (col1 || col2) (reverseXSpeed x)     

      col3 <- objectTopMapCollision x
      when col3 (reverseYSpeed x)
      col4 <- objectBottomMapCollision x
      when (col4) (do
        if( (n-100) < 0 )
          then (do
            setGameAttribute (Score (n-100) blocks)
            reverseYSpeed x)
          else(do 
               setGameAttribute (Score (n-100) blocks) 
               reverseYSpeed x))
      bar <- findObject "bar" "barGroup"
      col5 <- objectsCollision x bar
      let (_,vy) = getGameObjectSpeed x
      when (and [col5, vy < 0])  (do reverseYSpeed x
                                     setGameAttribute (Score (n + 10) blocks))
      checkBalls xs


    gameCycle :: IOGame GameAttribute () () TileAttribute ()
    gameCycle = do
      (Score n blocks) <- getGameAttribute
      balls <- getObjectsFromGroup "ballGroup"
      checkBalls balls
      printOnScreen (show n) TimesRoman24 (750,750) 0.0 0.0 0.0
      showFPS TimesRoman24 (w-40,0) 0.0 0.0 1.0
    
    f :: BreakoutTile
    f = (field, False, 0.0, NoTileAttribute)


    map1 :: BreakoutMap
    map1 = [[f]]