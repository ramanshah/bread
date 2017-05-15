import BreadData
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml.YamlLight as Y

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Nope!"

getFromArray :: Int -> Y.YamlLight -> Y.YamlLight
getFromArray i = (!! i) . fromJust . Y.unSeq

getFromMap :: String -> Y.YamlLight -> Y.YamlLight
getFromMap key = fromJust . Y.lookupYL (Y.YStr $ BS.pack key)

getString :: Y.YamlLight -> String
getString = BS.unpack . fromJust . Y.unStr

main :: IO ()
main = do
   x <- Y.parseYamlFile "./doc/examples/ciabatta.yml"
   putStrLn $ getString $ getFromMap "amount" $ getFromArray 2 $
     getFromMap "ingredients" $ getFromArray 1 $ x
