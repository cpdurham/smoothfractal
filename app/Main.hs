{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH

import Data.List (intercalate)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Directory
import System.FilePath
import System.Process
import System.IO
import System.Exit

getUsage :: IO String
getUsage =
  do
    prg <- getProgName
    return $ usageInfo prg options

data Options =
  Options
  { optFunction :: String
  , optConstants :: [(String,String)]
  , optAA :: Int
  , optRes :: Int
  , optFile :: FilePath
  } deriving Show

defaultFunction :: String
defaultFunction = "z^7 + (3-c)*z^3 + (c + c1)*z + c"

defaultConstants :: [(String,String)]
defaultConstants = [("c1","1 :+ 1")]

startOptions :: Options
startOptions =
  Options
  { optFunction = defaultFunction
  , optConstants = defaultConstants
  , optAA = 2
  , optRes = 1024
  , optFile = "test.bmp"
  }

options :: [OptDescr (Options -> IO Options)]
options =
  let
    quoteToTick :: Char -> Char
    quoteToTick '\"' = '\''
    quoteToTick c = c

    tickToQuote :: Char -> Char
    tickToQuote '\'' = '\"'
    tickToQuote c = c
  in
    [ Option "F" ["function"]
      (ReqArg
       (\arg opt -> return opt {optFunction = arg})
       "String")
      $ "Function String: " ++ show defaultFunction
    , Option "C" ["constants"]
      (ReqArg
       (\arg opt -> return opt {optConstants = read $ map tickToQuote arg})
       "[(String,String)]")
      $ "Constants String: " ++ (show $ map quoteToTick $ show defaultConstants)
    , Option "A" ["aa"]
      (ReqArg
       (\arg opt -> return opt {optAA = read arg})
       "Int")
      "Anti-Aliasing"
    , Option "R" ["res"]
      (ReqArg
       (\arg opt -> return opt {optRes = read arg})
       "Int")
      "Square Resolution"
    , Option "f" ["file"]
      (ReqArg
       (\arg opt -> return opt {optFile = arg})
       "FILE")
      "File Location"
    , Option "h" ["help"]
      (NoArg
       (\_ -> do
           prg <- getProgName
           hPutStrLn stderr $ usageInfo prg options
           exitWith ExitSuccess
       )
      ) "Show help"
    ]

myLocation :: String
myLocation =
  $(do
       dir <- runIO getCurrentDirectory
       litE $ stringL $ dir
   )

buildFunctionString :: Options -> String
buildFunctionString opts =
  let
    Options
      { optFunction = functionString
      , optConstants = constantsString
      } = opts

    makeConstants (name,def) =
      unlines $
      [ name ++ " = " ++ def
      , "{-# INLINE " ++ name ++ " #-}"
      ]
  in
    unlines $
    [ "module Func (func,order) where"
    , "import Data.Complex"
    , "import Degree"
    , ""
    , "func :: Complex Double -> Complex Double -> Complex Double"
    , "func = func' " ++ intercalate " " (map fst constantsString)
    , "{-# INLINE func #-}"
    , ""
    , "func' " ++ intercalate " " (map fst constantsString ++ ["c","z"]) ++ " = " ++ functionString
    , "{-# INLINE func' #-}"
    , ""
    , "order :: Double"
    , "order = fromIntegral $ getDegree $ func' " ++ unwords (replicate (length constantsString + 2) "(Degree 1 1)")
    , ""
    ] ++ map makeConstants constantsString

main :: IO ()
main =
  do
    args <- getArgs
    let (actions,_,_) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let
      funcLoc = myLocation </> "app/auxiliary/Func.hs"
      yamlLoc = myLocation </> "stack.yaml"
      funcString = buildFunctionString opts
      workingFuncString = buildFunctionString startOptions
      compileString = "stack --stack-yaml " ++ yamlLoc ++ " install :smoothfractal-child-exe"
      runString =
        "time smoothfractal-child-exe " ++
        show (optFile opts) ++ " " ++
        show (optRes opts) ++ " " ++
        show (optAA opts)
    writeFile funcLoc funcString
    putStrLn "Loading function.."
    (compileExitCode,o,e) <- readCreateProcessWithExitCode (shell compileString) ""
    putStrLn "Function loaded, running.."
    putStrLn e
    putStrLn o
    writeFile funcLoc workingFuncString
    case compileExitCode of
      ExitFailure _ ->
        do
          putStrLn $ "error: failed to parse " ++ show opts
      _ ->
        do
          (_,_,_,h) <- createProcess $ (shell runString)
          waitForProcess h >>= exitWith
