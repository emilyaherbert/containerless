import           Compiler
import           EvalJS
import           EvalTrace
import           Parser
import           Pretty
import           Syntax
import           Test.HUnit hiding (Label)

tests =
  TestList
    [ example1
    , example2
    , example3
    , example4
    , example5
    , example6
    , example7
    , example8
    , example9
    , example10
    , example11
    , fun1
    , fun2
    , fun3
    , fun4
    , fun5
    , fun6
    , fun7
    ]

basicExample label code =
  TestLabel label $
  TestCase $ do
    putStrLn ("\n\n--- " ++ label ++ " ---")
    let program = parseString code
    --putStrLn $ show program
    let compiled = compile program
    --putStrLn $ show compiled
    let state = eval compiled
    --putStrLn $ show $ store state
    let trace = current state
    putStrLn (prettyTrace trace)
    putStrLn $ "Final value of output = " ++ (show $ evalTrace trace)
    --putStrLn $ show $ cont state
    putStrLn ("--- End " ++ label ++ " ---")

example1 =
  basicExample
    "example1"
    "let x = 5;\
    \let y = x;\
    \output = y;"

example2 =
  basicExample
    "example2"
    "if (1 == 1) {\
    \   let x = 5;\
    \   output = x;\
    \} else {\
    \   let x = 50;\
    \   output = x;\
    \}"

example3 =
  basicExample
    "example3"
    "let x = 5;\
    \x = -5;\
    \output = x;"

example4 =
  basicExample
    "example4"
    "let x = 5;\
    \x = x - 1;\
    \output = x;"

example5 =
  basicExample
    "example5"
    "let x = 3;\
    \while (x > 0) {\
    \   x = x - 1;\
    \}\
    \output = x;"

example6 =
  basicExample
    "example6"
    "let x = 5;\
    \let y = 0;\
    \while (x > 0) {\
    \    x = x - 1;\
    \    if (x > 3) {\
    \        y = y + 10;\
    \    } else {\
    \        y = y + 1;\
    \    }\
    \}\
    \output = y;"

example7 =
  basicExample
    "example7"
    "let x = 5;\
    \if (1 == 1) {\
    \   x = 4;\
    \   x = 99;\
    \} else {\
    \   x = 50;\
    \}\
    \output = x;"

example8 =
  basicExample
    "example8"
    "let x = 5;\
    \let y = 0;\
    \while (x > 0) {\
    \   x = x - 1;\
    \   y = y + 1;\
    \}\
    \output = y;"

example9 =
  basicExample
    "example9"
    "foo: {\
    \   let x = 5;\
    \   let y = 10;\
    \}"

example10 =
  basicExample
    "example10"
    "let x = 0;\
    \let y = 0;\
    \let z = 0;\
    \foo: {\
    \   let x = 5;\
    \   let y = 10;\
    \   break foo;\
    \   let z = 99;\
    \};\
    \foo = z;"

example11 =
  basicExample
    "example11"
    "foo: {\
    \   let x = 5;\
    \   bar: {\
    \       let y = 10;\
    \       output = y;\
    \       break foo;\
    \       y = 7777;\
    \   }\
    \   let z = 99;\
    \}"

fun1 =
  basicExample
    "fun1"
    "let F = function (x) {\
    \   return x;\
    \};"

fun2 =
  basicExample
    "fun2"
    "let foo = 10;\
    \let F = function (x) {\
    \   return x;\
    \};"

fun3 =
  basicExample
    "fun3"
    "let foo = 10;\
    \let F = function (x) {\
    \   return x+3;\
    \};\
    \let bar = F(foo);\
    \output = bar;"

fun4 =
  basicExample
    "fun4"
    "let foo = 10;\
    \foo = 99;\
    \let F = function () {\
    \   return foo;\
    \};\
    \let bar = F();\
    \output = bar;"

fun5 =
  basicExample
    "fun5"
    "let foo = 10;\
    \let baz = 99;\
    \let F = function (x, y) {\
    \   foo = foo + foo;\
    \   return x + y;\
    \};\
    \let bar = F(foo, baz);\
    \let freddie = F(baz, foo);\
    \output = freddie;"

fun6 =
  basicExample
    "fun6"
    "let foo = 10;\
    \let makeAdder = function(x) {\
    \   let inner = function(y) {\
    \       foo = foo+1;\
    \       return x + y;\
    \   };\
    \   return inner;\
    \};\
    \let F = makeAdder(10);\
    \let bar = F(13);\
    \output = foo + bar;"

fun7 =
  basicExample
    "fun7"
    "let x = 10;\
    \let F = function(y) {\
    \   return x+y;\
    \};\
    \let foo = F(3);\
    \let bar = F(99);\
    \output = foo + bar;"

-- https://prettydiff.com/?m=beautify
main :: IO ()
main = do
  runTestTT tests
  putStrLn ("Done!")
    --let compiled = compile example1
    --putStrLn (show compiled)
    --putStrLn (show $ eval compiled)
