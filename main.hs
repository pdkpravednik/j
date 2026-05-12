import GenerisanjeLavirinta (generisiLavirint,ispisiLavirint)
import RjesavanjeLavirinta

main :: IO()
main = do 
    let dimenzije = (91,7,3)
    l <- generisiLavirint dimenzije
    ispisiLavirint dimenzije l