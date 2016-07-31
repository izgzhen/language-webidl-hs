import Test.HUnit
import Language.WebIDL.Parser
import Language.WebIDL.PPrint
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = runTestTT tests >>= print

tests :: Test
tests = TestList [ TestLabel "WebGL" (testIDL "examples/webgl.idl")
                 , TestLabel "File API" (testIDL "examples/fileapi.idl")
                 , TestLabel "Callback" (testIDL "examples/callback.idl") ]

testIDL :: FilePath -> Test
testIDL idlpath = TestCase $ do
    text <- liftIO $ readFile idlpath
    case parseIDL text of
        Right defs -> do
            let text' = unlines $ map printDef defs
            case parseIDL text' of
                Right defs' -> assertEqual "Definitions should be equal" defs defs'
                Left err -> assertFailure ("Incorrect parsing of regenerated file: " ++ show err ++ "\n" ++ text')
        Left err -> assertFailure ("Incorrect parsing of original file: " ++ show err)
