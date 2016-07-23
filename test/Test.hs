import Test.HUnit
import Language.WebIDL.Parser
import Language.WebIDL.PPrint
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = runTestTT (TestList [TestLabel "test WebGL" testWebGL]) >>= print

testWebGL :: Test
testWebGL = TestCase $ do
    text <- liftIO $ readFile "examples/webgl.idl"
    case parseIDL text of
        Right defs -> do
            let text' = unlines $ map printDef defs
            case parseIDL text' of
                Right defs' -> assertEqual "Definitions should be equal" defs defs'
                Left err -> assertFailure ("Incorrect parsing: " ++ show err ++ "\n" ++ text')
        Left err -> assertFailure ("Incorrect parsing: " ++ show err)
