import Test.Tasty
import Test.Tasty.Runners (defaultMainWithIngredients)
import Test.Tasty.Runners.AntXML

main :: IO ()
main = defaultMainWithIngredients (antXMLRunner:defaultIngredients) $ testGroup "Tests" [
  ]
