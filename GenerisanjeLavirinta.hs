module GenerisanjeLavirinta(generisiLavirint,ispisiLavirint) where
import qualified Data.Set as Set
import System.Random (StdGen, newStdGen, randomR)


-- Svaka celija je skup koordinata x,y,z
-- Lavirint je prestavljen kao skup slobodnih celija, neparne koordinate lavirinta su celije koje su stalno slobodne dok parne mogu biti zid
-- ili prohodna (zavisi da li ima prolaz izmedju stalno prohodnih celija) 

type Celija = (Int,Int,Int)
type Lavirint = Set.Set Celija      
type Dimenzije = (Int,Int,Int)      -- Sirina, Visina, Dubina

-- Svi moguci prohodni/slobodni susjedi neke celije
moguciSusjedi :: Celija -> Lavirint -> Dimenzije -> Bool -> [Celija]
moguciSusjedi (x,y,z) lavirint (sirina,visina,dubina) moguceVertikalno = 
    filter (\c -> not (Set.member c lavirint)) $
    filter unutarDimenzija kandidati
  where
    horizontalni = [(x+2, y, z), (x-2, y, z), (x, y+2, z), (x, y-2, z)]
    vertikalni   = [(x, y, z+2), (x, y, z-2)]
    kandidati    = if moguceVertikalno then horizontalni ++ vertikalni else horizontalni
    unutarDimenzija (nx, ny, nz) = nx > 0 && nx < sirina-1 && ny > 0 && ny < visina-1 && nz >= 0 && nz < dubina
    
generisiLavirint :: Dimenzije -> IO Lavirint
generisiLavirint (x,y,z) = 
    do
        gen <- newStdGen
        let pocetnaCelija = (1,1,0)
            dimenzije = (x,y,if even z then z*2 else z*2-1)
        return $ generisiHelper gen dimenzije [pocetnaCelija] (Set.singleton pocetnaCelija)


generisiHelper :: StdGen -> Dimenzije -> [Celija] -> Lavirint -> Lavirint
generisiHelper _ _ [] otvorene = otvorene
generisiHelper generator dimenzije (trenutnaCelija:stack) otvorene =
    let 
        (sansa, genNakonSanse) = randomR (0, 100) generator :: (Int, StdGen)
        mozeVertikalno = sansa < 30

        susjedi = moguciSusjedi trenutnaCelija otvorene dimenzije mozeVertikalno
    in if null susjedi 
      then generisiHelper generator dimenzije stack otvorene
      else 
          let (index, finalGen) = randomR (0, length susjedi - 1) genNakonSanse :: (Int, StdGen)
              sledeca = susjedi !! index
              probijenZid = nadjiSredinu trenutnaCelija sledeca
              novaOtvorena = Set.insert sledeca (Set.insert probijenZid otvorene)
          in generisiHelper finalGen dimenzije (sledeca:trenutnaCelija:stack) novaOtvorena
                  


nadjiSredinu :: Celija -> Celija -> Celija
nadjiSredinu (x1,y1,z1) (x2,y2,z2) = ((x1+x2) `div` 2, (y1+y2) `div` 2, (z1+z2) `div` 2)

ispisiLavirint :: Dimenzije -> Lavirint -> IO ()
ispisiLavirint (s,v,d) lavirint = mapM_ prikaziNivo [x|x<-[0..(if even d then d*2 else d*2-1)],even x]
  where
    prikaziNivo z = do
        putStrLn $ "\nNIVO: " ++ show (z `div` 2 + 1)
        mapM_ (putStrLn . renderRed z) [0..v-1]
    renderRed z y = [ if Set.member (x, y, z) lavirint 
                      then provjeriSusjede x y z 
                      else '▓' 
                    | x <- [0..s-1] ]
      where 
        provjeriSusjede x y z 
          | Set.member (x, y, z+1) lavirint && Set.member (x, y, z-1) lavirint = 'B' 
          | Set.member (x, y, z+1) lavirint = 'U' 
          | Set.member (x, y, z-1) lavirint = 'D'
          | otherwise = ' '
