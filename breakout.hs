module Main where
    import Graphics.UI.Fungen
    import Graphics.Rendering.OpenGL (GLdouble)
    import Control.Concurrent
    
    data GameState = Stage Int
    data GameAttribute = Score Int Int
    data TileAttribute = NoTileAttribute

    type Blocks = GameObject()
    type Ball = GameObject()
    type BreakoutAction a = IOGame GameAttribute () GameState TileAttribute a
    type BreakoutTile = Tile TileAttribute
    type BreakoutMap = TileMatrix TileAttribute

    firstWait = 10 * 10^6
    waitInMicros = 4 * 10^6
    liog = liftIOtoIOGame

    rows = 6
    columns = 6
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

    suicaFlag,mexicoFlag,belgicaFlag,brasilFlag,ball,field :: Int
    suicaFlag = 0
    mexicoFlag = 1
    belgicaFlag = 2
    brasilFlag = 3
    ball = 4
    field = 5

    main :: IO ()
    main = do
      let winConfig = ((100,80),(width,height),"Breakout 2.0")
          bmpList = [("suicaflag.bmp",     Nothing),
                     ("mexicoflag.bmp",    Nothing),
                     ("belgicaflag.bmp",   Nothing),
                     ("brasilflag.bmp",    Nothing),
                     ("ball.bmp",          magenta),
                     ("field.bmp",         Nothing)]

          gameMap = multiMap [(tileMap map1 w h)] 0 

          bar     = objectGroup "barGroup"  [createBar]
          ball    = objectGroup "ballGroup" createBall
          blocks  = objectGroup "blockGroup" initBlocks
          obstacle = objectGroup "obstacleGroup" [createObstacle]

          initAttributes = (Score 0 0)
          input = [
            (SpecialKey KeyRight, StillDown, moveBarToRight)
            ,(SpecialKey KeyLeft,  StillDown, moveBarToLeft)
            ,(Char 'q',            Press,     \_ _ -> funExit)
            ]
      ballValue <- newEmptyMVar
      putMVar ballValue 1
      increment <- newEmptyMVar
      putMVar increment False
      maximum <- newEmptyMVar
      putMVar maximum 2
      shouldWake <- newEmptyMVar
      putMVar shouldWake False
      forkIO $ allowBallLoop ballValue increment maximum
      funInit winConfig gameMap [bar,ball,blocks,obstacle] (Stage 0) initAttributes input (gameCycle ballValue increment maximum shouldWake) (Timer 16) bmpList
    
    createBlocks :: Int -> Int -> [Blocks]
    createBlocks row column
     | (row >= rows) = []
     | (column >= columns) = createBlocks (row+1) (-columns)
     | otherwise = do
      let offsetx = boundsx*2+5
      let offsety = boundsy*2+5
      let x = (w/1.84)+(offsetx * (fromIntegral column))
      let y = (h - 300)+((fromIntegral row) * offsety)
      let name = "block" ++ (show row) ++ (show column)
      (createBlockAt (x, y) (name):(createBlocks (row) (column+1)) )

    createBlockAt :: (GLdouble, GLdouble) -> String -> Blocks
    createBlockAt (pX, pY) name =
       let blockPoly = Tex (boundsx*2,boundsy*2) suicaFlag
       in object name blockPoly True (pX,pY) (0,0) ()
  
    initBlocks :: [Blocks]
    initBlocks = (createBlocks 0 (-columns) )
    
    --Spawn apenas dos blockos necessarios para o nivel
    spawnDesiredBlocks :: [Blocks] -> Int -> Int -> Int -> BreakoutAction ()
    spawnDesiredBlocks [] _ _ _ = return ()
    spawnDesiredBlocks (x:xs) rowGoal row column
      | (row >= rowGoal) = return()
      | (column >= columns) = spawnDesiredBlocks (x:xs) rowGoal (row+1) (-columns)
      | otherwise = do
        let offsetx = boundsx*2+5
        let offsety = boundsy*2+5
        let px = (w/1.84)+(offsetx * (fromIntegral column))
        let py = (h - 300)+((fromIntegral row) * offsety)
        setObjectAsleep False x
        spawnDesiredBlocks xs rowGoal row (column + 1)

    resetBlocks :: [Blocks] -> Int -> Int -> BreakoutAction ()
    resetBlocks [] _ _ = return ()
    resetBlocks (x:xs) row column
      | (row >= rows) = return()
      | (column >= columns) = resetBlocks (x:xs) (row+1) (-columns)
      | otherwise = do
        let offsetx = boundsx*2+5
        let offsety = boundsy*2+5
        let px = (w/1.84)+(offsetx * (fromIntegral column))
        let py = (h - 300)+((fromIntegral row) * offsety)
        setObjectAsleep True x
        resetBlocks xs row (column + 1)
    
    setFlags :: [Blocks] -> BreakoutAction ()
    setFlags [] = return ()
    setFlags (x:xs) = do
       state <- getGameState
       case state of
         Stage n -> case n of
                   1 -> (do 
                         setObjectCurrentPicture suicaFlag x)
                   2 -> (do
                         setObjectCurrentPicture mexicoFlag x)
                   3 -> (do 
                         setObjectCurrentPicture belgicaFlag x)
       setFlags xs                 

    createBall :: [GameObject ()]
    createBall =
      let ballPic = Tex (tileSize,tileSize) ball
      in [(object "ball1" ballPic True (400,450) (5,5) ()),
          (object "ball2" ballPic True (400,450) (5,5) ()),
          (object "ball3" ballPic True (400,450) (5*1.25,5*1.25) ())]

    createBar :: GameObject ()
    createBar =
      let barPic   = Tex (80,40) brasilFlag
      in object "bar" barPic False (w/2,30) (0,0) ()

    createObstacle :: GameObject ()
    createObstacle = let obstaclePic = [(-50,-7.5),(50,-7.5),(50,7.5),(-50,7.5)]
                         obstacle2 = Basic (Polyg obstaclePic 1.0 1.0 1.0 Filled)
                     in object "obstacle" obstacle2 True (w/2,(h/2)) (4,0) ()
    
    moveBarToRight :: Modifiers -> Position -> IOGame GameAttribute () GameState TileAttribute ()
    moveBarToRight _ _ = do
      obj     <- findObject "bar" "barGroup"
      (pX,pY) <- getObjectPosition obj
      (sX,_)  <- getObjectSize obj
      if (pX + (sX/2) + 5 <= w)
       then (setObjectPosition ((pX + 20),pY) obj)
       else (setObjectPosition ((w - (sX/2)),pY) obj)
    
    moveBarToLeft :: Modifiers -> Position -> IOGame GameAttribute () GameState TileAttribute ()
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
          setGameAttribute (Score (n+10) (blocks-1)))          
        else if((bY-pY)>=(pSizeY+bSizeY+vy))
          then(do
            reverseYSpeed ball
            setGameAttribute (Score (n+10) (blocks-1) ))
          else(do
            reverseXSpeed ball
            setGameAttribute (Score (n+10) (blocks-1)))       
        
    checkBalls :: MVar Int -> MVar Int -> MVar Bool -> [Ball] -> BreakoutAction ()
    checkBalls _ _ _ [] = return()
    checkBalls ballValue maximum shouldWake (x:xs) = do
      obstacle <- findObject "obstacle" "obstacleGroup"
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
                setGameState (Stage 1)
                tempVal2 <- liog $ (takeMVar maximum)
                liog $ putMVar maximum 2
                tempVal <- liog $ (takeMVar ballValue)
                liog $ putMVar ballValue 1
                switchLevel 1)
          else(do 
               setGameAttribute (Score (n-100) blocks) 
               reverseYSpeed x
               (posx,posy) <- getObjectPosition x
               setObjectPosition (posx,posy+20) x))
      bar <- findObject "bar" "barGroup"
      col5 <- objectsCollision x bar
      let (_,vy) = getGameObjectSpeed x
      when (and [col5, vy < 0])  (do reverseYSpeed x
                                     setGameAttribute (Score (n + 10) blocks))
      col6 <- objectsCollision x obstacle
      when (and [col6,vy > 0]) (do reverseYSpeed x)
      col7 <- objectLeftMapCollision obstacle
      col8 <- objectRightMapCollision obstacle
      when(col7 || col8) (do reverseXSpeed obstacle)

      checkBalls ballValue maximum shouldWake xs
      


    switchLevel :: Int -> BreakoutAction ()
    switchLevel n = do 
      flags <- getObjectsFromGroup "blockGroup"
      (Score y blocks) <- getGameAttribute
      setGameState (Stage n)
      state <- getGameState
      case state of
        Stage x -> case n of
                   1 -> (do 
                         resetBlocks flags 0 (-columns)
                         spawnDesiredBlocks flags 4 0 (-columns)
                         setFlags flags
                         setGameAttribute(Score 0 48)
                         ball1 <- findObject "ball1" "ballGroup"
                         ball2 <- findObject "ball2" "ballGroup"
                         ball3 <- findObject "ball3" "ballGroup"
                         setObjectPosition (450,400) ball1
                         setObjectPosition (450,400) ball2
                         setObjectPosition (450,400) ball3
                         obstacle <- findObject "obstacle" "obstacleGroup"
                         setObjectAsleep True obstacle
                         (speedX,speedY) <- getObjectSpeed ball1
                         setObjectSpeed (5,5) ball1
                         setObjectSpeed (5,5) ball2
                         setObjectAsleep True ball1
                         setObjectAsleep True ball2
                         setObjectAsleep True ball3)
                   2 -> (do 
                         spawnDesiredBlocks flags 5 0 (-columns)
                         setFlags flags
                         setGameAttribute(Score y 60)
                         ball1 <- findObject "ball1" "ballGroup"
                         ball2 <- findObject "ball2" "ballGroup"
                         ball3 <- findObject "ball3" "ballGroup"
                         setObjectPosition (450,400) ball1
                         setObjectPosition (450,400) ball2
                         setObjectPosition (450,400) ball3
                         (speedX,speedY) <- getObjectSpeed ball1
                         obstacle <- findObject "obstacle" "obstacleGroup"
                         setObjectAsleep True obstacle
                         setObjectSpeed (speedX*1.25,speedY*1.25) ball1
                         setObjectSpeed (speedX*1.25,speedY*1.25) ball2
                         setObjectAsleep True ball1
                         setObjectAsleep True ball2
                         setObjectAsleep True ball3)
                   3 -> (do
                         obstacle <- findObject "obstacle" "obstacleGroup"
                         ball3 <- findObject "ball3" "ballGroup"
                         ball2 <- findObject "ball2" "ballGroup"
                         ball1 <- findObject "ball1" "ballGroup"
                         setObjectPosition (450,400) ball1
                         setObjectPosition (450,400) ball2
                         setObjectPosition (450,400) ball3
                         setObjectAsleep False ball3
                         obstacle <- findObject "obstacle" "obstacleGroup"
                         setObjectAsleep True obstacle
                         spawnDesiredBlocks flags 6 0 (-columns)
                         setFlags flags
                         setGameAttribute(Score y 72)
                         setObjectAsleep True ball1
                         setObjectAsleep True ball2
                         setObjectAsleep True ball3)
                   _ -> return ()    

    gameCycle :: MVar Int -> MVar Bool -> MVar Int -> MVar Bool -> BreakoutAction ()
    gameCycle ballValue increment maximum shouldWake = do
      (Score n blocks) <- getGameAttribute
      balls <- getObjectsFromGroup "ballGroup"
      checkBalls ballValue maximum shouldWake balls
      state <- getGameState
      obstacle <- findObject "obstacle" "obstacleGroup"
      wake <- getObjectAsleep obstacle
      when(not wake)(do
        liog $ modifyMVar_ shouldWake (\x -> return (False))
        )
        
      when(blocks <= 0)(do
                        case state of
                          Stage n -> case n of 
                                  0 -> (do
                                       setGameState (Stage 1)
                                       tempVal2 <- liog $ (takeMVar maximum)
                                       liog $ putMVar maximum 2
                                       tempVal <- liog $ (takeMVar ballValue)
                                       liog $ putMVar ballValue 1
                                       switchLevel 1)
                                  1 -> (do
                                        setGameState (Stage 2)
                                        tempVal2 <- liog $ (takeMVar maximum)
                                        liog $ putMVar maximum 3
                                        tempVal <- liog $ (takeMVar ballValue)
                                        liog $ putMVar ballValue 1
                                        liftIOtoIOGame(putStrLn("LEVEL1"))  
                                        switchLevel 2)
                                  2 -> (do
                                        setGameState (Stage 3)
                                        tempVal2 <- liog $ (takeMVar maximum)
                                        liog $ putMVar maximum 3
                                        tempVal <- liog $ (takeMVar ballValue)
                                        liog $ putMVar ballValue 1
                                        liog $ forkIO (allowObstacleDelayed shouldWake)
                                        switchLevel 3)
                                  3 -> funExit)         

      tryWakingBall ballValue
      tryWakingObstacle shouldWake
      liog $ (modifyMVar_ increment (\x -> return(True)))
      printOnScreen (("Score: ") ++ show n) TimesRoman24 (620,800) 1.0 1.0 1.0
      printOnScreen (("Blocks Left: ") ++ show blocks) TimesRoman24 (0,28) 1.0 1.0 1.0
      showFPS TimesRoman24 (w-40,0) 0.0 0.0 1.0
    
    f :: BreakoutTile
    f = (field, False, 0.0, NoTileAttribute)


    map1 :: BreakoutMap
    map1 = [[f]]

    --Threading
    allowBallLoop :: MVar Int -> MVar Bool -> MVar Int -> IO ()
    allowBallLoop ballValue increment maximum = do
      value <- readMVar ballValue
      inc <- readMVar increment
      max <- readMVar maximum
      threadDelay waitInMicros
      if (value == max || inc == False) then return ()
      else (do
        value <- takeMVar ballValue
        putMVar ballValue (value + 1)
        )
      allowBallLoop ballValue increment maximum 
    
    tryWakingBall :: MVar Int -> BreakoutAction ()
    tryWakingBall ballValue = do
      value <- liog (readMVar ballValue)
      ball <- findObject ("ball" ++ (show value)) "ballGroup"
      asleep <- getObjectAsleep ball
      when (asleep) (do
        liog $ print ((show value) ++ " acordando")
        setObjectAsleep False ball
        )

    tryWakingObstacle :: MVar Bool -> BreakoutAction ()
    tryWakingObstacle shouldWake = do
      sw <- liog $ readMVar shouldWake
      when (sw) (do
        obstacle <- findObject "obstacle" "obstacleGroup"
        setObjectAsleep False obstacle
        )

    allowObstacleDelayed :: MVar Bool -> IO ()
    allowObstacleDelayed shouldWake = do
      threadDelay waitInMicros
      modifyMVar_ shouldWake (\x -> return (True))
    --Fim de Threading
