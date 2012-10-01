-- compile with -rtsopts, -threaded, and -O for best results, and play
-- with the rts opts a little

import           Control.Applicative
import           Control.Concurrent
import           Data.Char
import           Data.List
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Graphics.GD hiding (Point)
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Parsec hiding (many, optional, (<|>), space, spaces)
import           Text.Printf

type Point     = Int
type TeamIndex = Int

data Stage a = Stage {width    :: Int,
                      height   :: Int,
                      alive    :: IntMap TeamIndex,       -- ^ Point     -> TeamIndex
                      dying    :: IntMap TeamIndex,       -- ^ Point     -> TeamIndex
                      teams    :: IntMap (Maybe a, Int)}  -- ^ TeamIndex -> (team object or neutral, number of instances)
                                                          -- NB: The neutral team should always be index -1.

pointToTuple :: Stage a -> Point -> (Int,Int)
tupleToPoint :: Stage a -> (Int,Int) -> Point

pointToTuple s pt    = (x,y) where (y,x) = pt `quotRem` width s
tupleToPoint s (x,y) = y * width s + x

neighbors :: Stage a -> Point -> [Point]
neighbors s pt = [correct (x+x',y+y') | y' <- [-1,0,1],
                                        x' <- [-1,0,1],
                                        (x',y') /= (0,0)]
  where (x,y) = pointToTuple s pt
        correct (x,y)
          | x >= width  s = correct (x - width  s, y)
          | y >= height s = correct (x, y - height s)
          | x < 0         = correct (x + width  s, y)
          | y < 0         = correct (x, y + height s)
          | otherwise     = tupleToPoint s (x,y)

neighborTeams :: Stage a -> Point -> [TeamIndex]
neighborTeams s pt = [t | Just t <- map (flip IntMap.lookup (alive s)) (neighbors s pt)]

isDead :: Stage a -> Point -> Bool
isDead s c = c `IntMap.notMember` alive s && c `IntMap.notMember` dying s

step o = IntSet.fold update (o { alive = IntMap.empty, dying = alive o, teams = teamsZero }) deadNeighbors
  where teamsZero       = IntMap.map (\(a,n) -> (a,0)) (teams o)
        deadNeighbors   = IntSet.fold (IntSet.union . IntSet.filter (isDead o) . IntSet.fromList
                                       . neighbors o) IntSet.empty $ IntMap.keysSet $ alive o
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

writer sf c             = readChan c >>= f
  where f (Left  d)     = do let Just win = winner d
                             putStrLn $ "\nwinner: " ++
                               case win of
                                 Just t  -> show t
                                 Nothing -> "nobody; it's a tie!"
        f (Right (i,s)) = do status i s
                             sf     i s
                             writer sf c

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

wnull i s = return ()

process c i s = do writeChan c $! Right (i,s)
                   if isWon s
                      then writeChan c $ Left s
                      else process c (i+1) (step s)

main = do args <- getArgs
          if length args < 3
             then hPutStrLn stderr "usage: brain-battle (png|null) <sourcefile> <outputdir>" >> exitFailure
             else do let (mode:ss:o:_) = args
                     s <- fmap (readBBB ss) (readFile ss)
                     c <- newChan :: IO (Chan (Either (Stage (Int,Int,Int)) (Int,Stage (Int,Int,Int))))
                     forkIO $ process c (0::Int) $! s
                     case mode of
                       "png"  -> writer     (png o) c
                       "null" -> writer     wnull   c
                       _      -> hPutStrLn  stderr "mode not recognized; should be png or null" >> exitFailure