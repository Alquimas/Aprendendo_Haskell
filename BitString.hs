import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO

intListToByteString :: [Int] -> L.ByteString
intListToByteString = L8.pack . map (toEnum . fromIntegral)

create_list :: Int -> [Int]
create_list 0 = []
create_list x = 0:0:0 : (create_list (x-1))

create_matrix :: Int -> [[Int]]
create_matrix 0 = []
create_matrix x = create_list 20 : create_matrix (x-1)

writeByteStringsToFile :: Handle -> [L.ByteString] -> IO ()
writeByteStringsToFile outh byteStrings = mapM_ (L.hPut outh) byteStrings

pack_matrix :: [L.ByteString]
pack_matrix = map intListToByteString $ create_matrix 20

main :: IO ()
main = do
    outh <- openFile "bytemap.ppm" WriteMode
    hPutStrLn outh "P6\n20 20\n255"
    writeByteStringsToFile outh pack_matrix
    hClose outh 


