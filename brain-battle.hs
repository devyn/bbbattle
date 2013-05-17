-- compile with -rtsopts, -threaded, and -O for best results, and play
-- with the rts opts a little

import           Control.Applicative
import           Control.Concurrent
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
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

pointToTuple :: Stage a -> Point -> (Int,Int)
tupleToPoint :: Stage a -> (Int,Int) -> Point

pointToTuple s pt    = (x,y) where y = pt `shiftR` 16
                                   x = pt .&.      65535
tupleToPoint s (x,y) = (y `shiftL` 16) .|. x

addNeighborsToSet s set pt = i (x+1,y)
                           . i (x,y+1)
                           . i (x-1,y)
                           . i (x,y-1)
                           . i (x+1,y+1)
                           . i (x-1,y-1)
                           . i (x+1,y-1)
                           . i (x-1,y+1) $ set
  where (x,y)   = pointToTuple s pt
        i (x,y) = IntSet.insert (tupleToPoint s (x `mod` width s, y `mod` height s))

neighbors :: Stage a -> Point -> [Point]
neighbors s pt = map correct coords
  where (x,y) = pointToTuple s pt
        correct (x,y) = tupleToPoint s (x `mod` width s, y `mod` height s)
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
        isInRange pt    = let (x,y) = pointToTuple o pt
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

readBBB :: FilePath -> String -> Stage (Int,Int,Int)

readBBB fn s = either (error.show) id $ parse (optional space *> bbbParser <* optional space <* eof) fn s

bbbParser = f <$> bbbDims <*> (space *> many bbbTeam)
  where f (w,h) tms = let its     = zip [0..] tms
                          al      = concatMap (\(x,(t,(ps,_))) -> [(tupleToPoint stg p,x) | p <- ps]) its
                          dy      = concatMap (\(x,(t,(_,ps))) -> [(tupleToPoint stg p,x) | p <- ps]) its
                          ts      = nt :  map (\(x,(t,(ps,_))) -> (x,(Just t,length al))) its
                          nt      = ((-1),(Nothing,0))
                          stg     = Stage { width  = w
                                          , height = h
                                          , teams  = IntMap.fromList ts
                                          , alive  = IntMap.fromList al
                                          , dying  = IntMap.fromList dy } in stg

bbbDims = (,) <$> (num <* space) <*> num

num = read <$> many1 (satisfy isDigit)

space = many1 (satisfy isSpace)

spaces p = optional space *> p <* optional space

color = spaces ((,,) <$> (num <* space) <*> (num <* space) <*> num)

bbbTeam = spaces ((,) <$> (color <* char ':') <*> ((,) <$> bbbPointList 'a' <*> bbbPointList 'd'))

bbbPointList c = spaces (char c *> (p `sepBy` space) <* char '.')
  where p      = (,) <$> (num <* optional space <* char ',') <*> (optional space *> num)

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
  where convert (pt,ti) = (pointToTuple s pt
                          ,maybe Nothing fst (IntMap.lookup ti (teams s)))
        pxa ((x,y),v) =
          case v of
            Just (r,g,b) -> setPixel (x,y) (rgb r   g   b)   img
            Nothing      -> setPixel (x,y) (rgb 160 160 160) img
        pxd ((x,y),v) =
          case v of
            Just (r,g,b) -> setPixel (x,y) (rgb (r`div`2) (g`div`2) (b`div`2)) img
            Nothing      -> setPixel (x,y) (rgb 80        80        80)        img

png o i s = withImage (newImage (width s, height s)) $ \ img ->
              do stageToImage s img
                 savePngFile (o </> printf "%04i.png" i) img

createColor screen r g b = SDL.mapRGB (SDL.surfaceGetPixelFormat screen) (fromIntegral r) (fromIntegral g) (fromIntegral b)

sdl screen i s = do black <- createColor screen 0 0 0
                    SDL.fillRect screen (Just (SDL.Rect 0 0 (width s) (height s))) black -- blank
                    mapM_ (pxa . convert) (IntMap.toList $ alive s)
                    mapM_ (pxd . convert) (IntMap.toList $ dying s)
                    SDL.flip screen
  where convert (pt,ti) = (pointToTuple s pt
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
                       "png"  -> writer     (png o)  eic pcos (0::Int)
                       "sdl"  -> do SDL.init [SDL.InitEverything]
                                    SDL.setVideoMode (width s) (height s) 32 []
                                    sc <- SDL.getVideoSurface
                                    writer  (sdl sc) eic pcos (0::Int)
                       "null" -> writer     wnull    eic pcos (0::Int)
                       _      -> hPutStrLn  stderr "mode not recognized; should be png, sdl or null" >> exitFailure
