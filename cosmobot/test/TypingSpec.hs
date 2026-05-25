module Main (main) where

import Bot.Handler.Typing.Typst
import Bot.Prelude
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "typing"
      [ testCase "rank cells are emitted as Typst string content" testRankCellsUseTextStrings
      , testCase "Typst strings escape only string syntax" testTypstStringEscapesStringSyntax
      ]

testRankCellsUseTextStrings :: IO ()
testRankCellsUseTextStrings = do
  let document = typstDocument "2026-05-14锦标赛成绩" [["用户名", "输入法"], ["魔然@rime-mkm", "[#foo] $x_1$"]]
  assertBool "keeps @ inside a string literal" ("text(\"魔然@rime-mkm\")" `Text.isInfixOf` document)
  assertBool "does not emit markup cells that can parse @ as a label reference" (not ("[魔然@rime-mkm]" `Text.isInfixOf` document))
  assertBool "keeps markup-like text inside a string literal" ("text(\"[#foo] $x_1$\")" `Text.isInfixOf` document)

testTypstStringEscapesStringSyntax :: IO ()
testTypstStringEscapesStringSyntax =
  typstString "a\\b\"c\n下一行" @?= "\"a\\\\b\\\"c\\n下一行\""
