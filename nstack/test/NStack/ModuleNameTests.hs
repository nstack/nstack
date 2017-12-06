module NStack.ModuleNameTests where

import Data.Text

import Test.Tasty
import Test.Tasty.HUnit

import NStack.Auth
import NStack.Module.Name (ModuleURI(..), NSUri(..))
import NStack.Module.Version (SemVer(..), FuzzyRelease(..))
import NStack.Module.Parser

{-
   Module Names take the form:

   `registry/author/modulename:version`

   where
     registry = period-separated list of alpa-numeric (or underscore) words of length at least 2, e.g.
                 foo.bar
                 foo.bar.baz
     author = single alpha-numeric (or underscore) word, without periods, e.g.
                 nick
                 ed
                 mandeep
                 my_name
                 name32
     modulename = period-separated list of Titlecase alpha-numeric (or underscore) words, e.g.
                 Foo
                 Foo.Bar
                 Foo.Bar.Baz
                 Foo_Bar
     version = period separated triple of integer values
                 (SemVer major, minor, patch, see http://semver.org),
                 with an optional "-SNAPSHOT" suffix, e.g.
                   1.0.0
                   1.2.3
                   1.2.3-SNAPSHOT

                e.g.
                   registry.nstack.com/my_name/Foo:1.2.3-SNAPSHOT
   -}

moduleNameTests :: TestTree
moduleNameTests = testGroup "ModuleName Tests" [
    testCase "Registry,Author,Name" $ "registry.nstack.com/nstack/Foo:1.0.0"      ~>  ("registry.nstack.com", "nstack",     "Foo", (1,0,0)),
    testCase "Author,Name"          $ "nstack/Foo:1.0.1"                          ~>  ("registry.nstack.com", "nstack",     "Foo", (1,0,1)),
    testCase "Name"                 $ "Foo:1.2.0"                                 ~>  ("registry.nstack.com", "nstack",     "Foo", (1,2,0)),
    testCase "Numbers"              $ "registry.nstack.com/401/Foo:1.0.0"         ~>  ("registry.nstack.com", "401",        "Foo", (1,0,0)),
    testCase "Underscores"          $ "registry.nstack.com/nstack/Foo_bar:1.0.0"  ~>  ("registry.nstack.com", "nstack",     "Foo_bar", (1,0,0)),
    testCase "Underscores"          $ "registry.nstack.com/n_stack/Foo_bar:1.0.0" ~>  ("registry.nstack.com", "n_stack",    "Foo_bar", (1,0,0)),
    testCase "Short registry"       $ "registry/nstack/Foo:1.2.0"                 ~/> Fail,
    testCase "Too Many Separators"  $ "too/many/separators/Foo:1.2.0"             ~/> Fail,
    testCase "Not Titlecased ModName" $ "registry.nstack.com/nstack/foo:1.2.0"    ~/> Fail,
    testCase "Registry,Name"        $ "registry.nstack.com/Foo:1.2.0"             ~/> Fail,
    testCase "Bad author"           $ "registry.nstack.com/bad.author/Foo:1.2.0"  ~/> Fail
  ]

(~>) :: Text -> (Text, Text, Text, (Integer, Integer, Integer)) -> IO ()
(~>) src (reg, author, name, (maj, mnr, pat)) = either assertFailure (assertEqual "Should be equal" expected) $ parseModuleName src
  where expected = (ModuleURI (uri reg)
                              (UserName author)
                              (uri name)
                              (SemVer maj mnr pat FRelease))

(~/>) :: Text -> Fail -> IO ()
(~/>) src _ = either (const $ assertBool "Did not fail" True) (const $ assertFailure "Should fail") $ parseModuleName src

uri :: Text -> NSUri
uri = NSUri . splitOn "."

data Fail = Fail
