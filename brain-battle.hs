-- compile with -rtsopts, -threaded, and -O for best results, and play
-- with the rts opts a little

import           Control.Applicative
import           Control.Concurrent
import           Data.Binary
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Char
import           Data.List
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Word
import           Graphics.GD hiding (Point)
import qualified Graphics.UI.SDL as SDL
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Parsec hiding (many, optional, (<|>), space, spaces)
import           Text.Printf

type Point     = Int
type TeamIndex = Int

data Stage a = Stage {width    :: !Int,
                      height   :: !Int,
                      alive    :: IntMap TeamIndex,       -- ^ Point     -> TeamIndex
                      dying    :: IntMap TeamIndex,       -- ^ Point     -> TeamIndex
                      teams    :: IntMap (Maybe a, Int)}  -- ^ TeamIndex -> (team object or neutral, number of instances)
                                                          -- NB: The neutral team should always be index -1.

pointToTuple :: Point -> (Int,Int)
tupleToPoint :: (Int,Int) -> Point

pointToTuple pt    = (x,y) where y = pt `shiftR` 16
                                 x = pt .&.      65535
tupleToPoint (x,y) = (y `shiftL` 16) .|. x

addNeighborsToSet s set pt = i (x+1,y)
                           . i (x,y+1)
                           . i (x-1,y)
                           . i (x,y-1)
                           . i (x+1,y+1)
                           . i (x-1,y-1)
                           . i (x+1,y-1)
                           . i (x-1,y+1) $ set
  where (x,y)   = pointToTuple pt
        i (x,y) = IntSet.insert (tupleToPoint (x `mod` width s, y `mod` height s))

neighbors :: Stage a -> Point -> [Point]
neighbors s pt = map correct coords
  where (x,y) = pointToTuple pt
        correct (x,y) = tupleToPoint (x `mod` width s, y `mod` height s)
        coords = [(x+1,y)
                 ,(x,y+1)
                 ,(x-1,y)
                 ,(x,y-1)
                 ,(x+1,y+1)
                 ,(x-1,y+1)
                 ,(x+1,y-1)
                 ,(x-1,y-1)]

neighborTeams :: Stage a -> Point -> [TeamIndex]
neighborTeams s pt = [t | Just t <- map (flip IntMap.lookup (alive s)) (neighbors s pt)]

isDead :: Stage a -> Point -> Bool
isDead s c = c `IntMap.notMember` alive s && c `IntMap.notMember` dying s

step o (minX,minY,maxX,maxY) = IntSet.fold update (o { alive = IntMap.empty, dying = alive o, teams = teamsZero }) deadNeighbors
  where teamsZero       = IntMap.map (\(a,n) -> (a,0)) (teams o)
        deadNeighbors   = IntSet.filter (isDead o) .
                            IntSet.foldl (addNeighborsToSet o)
                                          IntSet.empty . IntSet.filter isInRange . IntMap.keysSet $ alive o
        isInRange pt    = let (x,y) = pointToTuple pt
                          in  x >= minX && x < maxX && y >= minY && y < maxY
        incsnd (a,n)    = (a,n+1)
        add    pt s t   = s { teams = IntMap.adjust incsnd t (teams s) ,
                              alive = IntMap.insert pt     t (alive s) }
        update pt s     = case neighborTeams o pt of
                            [t1,t2]
                              | t1 == t2  -> add pt s  t1
                              | otherwise -> add pt s (-1)
                            _ -> s

isWon s = case winner s of
            Just _  -> True
            Nothing -> False

winner s = case IntMap.size ts of
             0 -> Just Nothing
             1 -> Just . fst . snd . head $ IntMap.assocs ts
             _ -> Nothing
  where ts = IntMap.filter ((/= 0).snd) $ IntMap.delete (-1) (teams s)

readBBB :: FilePath -> String -> Stage (Word8,Word8,Word8)

readBBB fn s = either (error.show) id $ parse (optional space *> bbbParser <* optional space <* eof) fn s

bbbParser = f <$> bbbDims <*> (space *> many bbbTeam)
  where f (w,h) tms = let its     = zip [0..] tms
                          al      = concatMap (\(x,(t,(ps,_))) -> [(tupleToPoint p, x) | p <- ps]) its
                          dy      = concatMap (\(x,(t,(_,ps))) -> [(tupleToPoint p, x) | p <- ps]) its
                          ts      = nt :  map (\(x,(t,(ps,_))) -> (x,(Just t,length al))) its
                          nt      = ((-1),(Nothing,0))
                          stg     = Stage { width  = w
                                          , height = h
                                          , teams  = IntMap.fromList ts
                                          , alive  = IntMap.fromList al
                                          , dying  = IntMap.fromList dy } in stg

bbbDims = (,) <$> (num <* space) <*> num

num :: (Read a, Num a) => Parsec [Char] () a
num = read <$> many1 (satisfy isDigit)

space = many1 (satisfy isSpace)

spaces p = optional space *> p <* optional space

color = spaces ((,,) <$> (num <* space) <*> (num <* space) <*> num)

bbbTeam = spaces ((,) <$> (color <* char ':') <*> ((,) <$> bbbPointList 'a' <*> bbbPointList 'd'))

bbbPointList c = spaces (char c *> (p `sepBy` space) <* char '.')
  where p      = (,) <$> (num <* optional space <* char ',') <*> (optional space *> num)

type ColorStage = Stage (Word8,Word8,Word8)

writer :: (Int -> ColorStage -> IO ()) -> Chan (Maybe ColorStage) -> [Chan ColorStage] -> Int -> IO ()

writer sf pc pcos i =
  do s <- foldl1 (\ s s' -> s { alive = alive s `IntMap.union` alive s'
                              , teams = IntMap.mapWithKey (\ k (t,m) -> let Just (_,n) = IntMap.lookup k (teams s') in (t, n + m))
                                                          (teams s) }) <$> mapM (readChan) pcos
     status i s
     case winner s of
          Just (Just t) -> writeChan pc Nothing  >> sf i s >> putStrLn ("\nwinner: " ++ show t)
          Just Nothing  -> writeChan pc Nothing  >> sf i s >> putStrLn ("\nwinner: nobody; it's a tie!")
          Nothing       -> writeChan pc (Just s) >> sf i s >> writer sf pc pcos (i+1)

status i s = do let al       = IntMap.size (alive s)
                    dy       = IntMap.size (dying s)
                    de       = width s * height s - al - dy
                    pr (a,b) = maybe "neutral" show a ++ ": " ++ show b
                putStr $ "\ESC[1G\ESC[Kgeneration " ++ show i ++ " (" ++ show al
                      ++ " alive, " ++ show dy ++ " dying, " ++ show de ++ " dead -- "
                      ++ (intercalate "; " . map pr . IntMap.elems $ teams s) ++ ")"
                hFlush stdout -- ensure the status gets displayed

stageToImage s img =
  do fillImage (rgb 0 0 0) img
     mapM_ (pxa . convert) (IntMap.toList $ alive s)
     mapM_ (pxd . convert) (IntMap.toList $ dying s)
     return img
  where convert (pt,ti) = (pointToTuple pt
                          ,maybe Nothing fst (IntMap.lookup ti (teams s)))
        rgb' r g b    = rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)
        pxa ((x,y),v) =
          case v of
            Just (r,g,b) -> setPixel (x,y) (rgb' r   g   b)   img
            Nothing      -> setPixel (x,y) (rgb  160 160 160) img
        pxd ((x,y),v) =
          case v of
            Just (r,g,b) -> setPixel (x,y) (rgb' (r`div`2) (g`div`2) (b`div`2)) img
            Nothing      -> setPixel (x,y) (rgb  80        80        80)        img

png o i s = withImage (newImage (width s, height s)) $ \ img ->
              do stageToImage s img
                 savePngFile (o </> printf "%04i.png" i) img

createColor screen r g b = SDL.mapRGB (SDL.surfaceGetPixelFormat screen) r g b

sdl screen i s = do black <- createColor screen 0 0 0
                    SDL.fillRect screen (Just (SDL.Rect 0 0 (width s) (height s))) black -- blank
                    mapM_ (pxa . convert) (IntMap.toList $ alive s)
                    mapM_ (pxd . convert) (IntMap.toList $ dying s)
                    SDL.flip screen
  where convert (pt,ti) = (pointToTuple pt
                          ,maybe Nothing fst (IntMap.lookup ti (teams s)))
        pxRect x y = Just (SDL.Rect (fromIntegral x) (fromIntegral y) 1 1)
        pxa ((x,y),v) =
          case v of
            Just (r,g,b) -> createColor screen r   g   b   >>= SDL.fillRect screen (pxRect x y)
            Nothing      -> createColor screen 160 160 160 >>= SDL.fillRect screen (pxRect x y)
        pxd ((x,y),v) =
          case v of
            Just (r,g,b) -> createColor screen (r`div`2) (g`div`2) (b`div`2) >>= SDL.fillRect screen (pxRect x y)
            Nothing      -> createColor screen 80        80        80        >>= SDL.fillRect screen (pxRect x y)

invertIntMap = IntMap.foldlWithKey insert IntMap.empty
  where insert m k v = IntMap.alter (Just . maybe (IntSet.singleton k) (IntSet.insert k)) v m

groupByTeam s = flip map (IntSet.toList $ IntMap.keysSet al
                           `IntSet.union` IntMap.keysSet dy) $ \ t ->
                  ( t
                  , maybe IntSet.empty id (IntMap.lookup t al)
                  , maybe IntSet.empty id (IntMap.lookup t dy)
                  )
  where al = invertIntMap (alive s)
        dy = invertIntMap (dying s)

putCellSet set = flip mapM_ (IntMap.toList yxmap) $ \ (y,xs) ->
                   do putWord16be $ fromIntegral y
                      putWord16be . fromIntegral $ IntSet.size xs
                      mapM_ (putWord16be . fromIntegral) $ IntSet.toList xs

  where yxmap               = foldl insertPoint IntMap.empty . map pointToTuple $ IntSet.toList set
        insertPoint m (x,y) = IntMap.alter (Just . maybe (IntSet.singleton x) (IntSet.insert x)) y m

putGeneration i s = do putWord8    $ c 'g'
                       putWord32be $ fromIntegral i -- generation number

                       flip mapM_ (groupByTeam s) $ \ (t,a,d) ->
                         do putWord8    $ c 't'
                            putWord16be $ fromIntegral t

                            putWord8    $ c 'a'
                            putCellSet  a

                            putWord8    $ c 'd'
                            putCellSet  d
  where c = fromIntegral . ord

putHeader s = do putByteString $ BSC.pack "bbbout1:"
                 putWord16be   . fromIntegral $ width s
                 putWord16be   . fromIntegral $ height s

                 putWord8      $ c 'T'
                 putWord16be   . fromIntegral . subtract 1 . IntMap.size $ teams s
                 flip mapM_ [(t,c) | (t,(Just c,_)) <- IntMap.toList (teams s)] $ \ (t,(r,g,b)) ->
                   do putWord16be $ fromIntegral t
                      putWord8    r
                      putWord8    g
                      putWord8    b
  where c = fromIntegral . ord

bbbout f i s = B.hPut f . runPut $ putGeneration i s

wnull i s = return ()

process ic oc range = do s <- readChan ic
                         case s of
                              Nothing -> return ()
                              Just s  -> do writeChan oc $! step s range
                                            process ic oc range

main = do args <- getArgs
          if length args < 3
             then hPutStrLn stderr "usage: brain-battle (png|null) <sourcefile> <outputdir>" >> exitFailure
             else do let (mode:ss:o:_) = args
                     s <- fmap (readBBB ss) (readFile ss)

                     caps <- getNumCapabilities

                     eic <- newChan
                     ics <- (eic :) <$> mapM (const (dupChan eic)) [0..(caps-2)]
                     pcos <- mapM (const newChan) [0..(caps-1)]

                     if caps `mod` 2 == 0
                        then let csize = width s `div` (caps `div` 2) + 1
                             in  mapM (\ n -> do forkIO $ process (ics !! (n*2))   (pcos !! (n*2))   (n*csize,0,((n+1)*csize),height s `div` 2)
                                                 forkIO $ process (ics !! (n*2+1)) (pcos !! (n*2+1)) (n*csize,height s `div` 2,((n+1)*csize),height s))
                                      [0..((caps `div` 2)-1)]
                        else let csize = width s `div` caps + 1
                             in  mapM (\ (oc,n) -> forkIO $ process (ics !! n) oc (n*csize,0,((n+1)*csize),height s))
                                      (zip pcos [0..(caps-1)])

                     writeChan eic (Just s)
                     case mode of
                       "png"    -> writer     (png o)  eic pcos (0::Int)
                       "sdl"    -> do SDL.init [SDL.InitEverything]
                                      SDL.setVideoMode (width s) (height s) 32 []
                                      sc <- SDL.getVideoSurface
                                      writer  (sdl sc) eic pcos (0::Int)
                       "bbbout" -> withFile o WriteMode $ \ f ->
                                     do B.hPut f . runPut $ putHeader s
                                        B.hPut f . runPut $ putGeneration 0 s
                                        writer (bbbout f) eic pcos (0::Int)
                       "null"   -> writer     wnull    eic pcos (0::Int)
                       _        -> hPutStrLn  stderr "mode not recognized; should be png, sdl, bbbout or null" >> exitFailure
