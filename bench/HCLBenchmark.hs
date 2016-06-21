import           Control.Arrow    ((>>>))
import           Control.Monad
import           Criterion.Main
import           Data.HCL
import qualified Data.Text.IO     as Text (readFile)
import           System.Directory (getDirectoryContents)
import           System.FilePath  (takeExtension, (</>))

main :: IO ()
main = do
    allFixtures <-
        filter (/= "unterminated_brace.hcl") .
        filter (/= "unterminated_block_comment.hcl") .
        filter (/= "multiline_no_marker.hcl") .
        filter (/= "multiline_bad.hcl") .
        filter (takeExtension >>> (== ".hcl")) <$>
        getDirectoryContents "./test-fixtures"

    bs <- forM allFixtures $ \fixture -> do
        print fixture
        input <- Text.readFile ("./test-fixtures" </> fixture)
        input `seq` return ()
        return $ bench fixture $ nf
            (\i -> let Right h = parseHCL "" i in h) input

    defaultMain [ bgroup "parseHCL" bs
                ]
