import qualified Data.Conf             as Conf
import qualified Data.Conf.PrettyPrint as Conf
import qualified Data.Text.IO          as Text (readFile)
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure)
import           System.IO
import qualified Text.Megaparsec       as Megaparsec

main :: IO ()
main = do
    as <- getArgs
    case as of
        (filePath:_) -> do
            contents <- Text.readFile filePath
            case Megaparsec.parse Conf.conf filePath contents of
                Left err -> hPrint stderr err
                Right ast -> print (Conf.pPrint ast)
        _ -> do
            hPutStrLn stderr "Usage: conffmt <conf-file>"
            exitFailure
