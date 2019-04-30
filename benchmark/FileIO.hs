-- |
-- Module      : Main
-- Copyright   : (c) 2019 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}

import Data.Char (ord, chr)
import qualified Streamly.Prelude as S
import qualified Streamly.FileIO as IO
import qualified Streamly.Array as A
import qualified Streamly.Fold as FL

import Control.DeepSeq (NFData)
import Gauge
import System.Process.Typed (shell, runProcess_)
import System.IO (openFile, IOMode(..), Handle, hClose)
#ifdef DEVBUILD
import System.IO (hSeek, SeekMode(..))
#endif
import Data.IORef

foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int

{-# INLINE isSpace #-}
isSpace                 :: Char -> Bool
-- isSpace includes non-breaking space
-- The magic 0x377 isn't really that magical. As of 2014, all the codepoints
-- at or below 0x377 have been assigned, so we shouldn't have to worry about
-- any new spaces appearing below there. It would probably be best to
-- use branchless ||, but currently the eqLit transformation will undo that,
-- so we'll do it like this until there's a way around that.
isSpace c
  | uc <= 0x377 = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
  | otherwise = iswspace (ord c) /= 0
  where
    uc = fromIntegral (ord c) :: Word

data Handles = Handles Handle Handle

scratchDir :: String
scratchDir = "benchmark/scratch/"

infile :: String
infile = scratchDir ++ "in-100MB.txt"

outfile :: String
outfile = scratchDir ++ "out.txt"

blockSize, blockCount, fileSize :: Int
blockSize = 32768
blockCount = 3200
fileSize = blockSize * blockCount

main :: IO ()
main = do
    let cmd = "mkdir -p " ++ scratchDir
                ++ "; test -e " ++ infile
                ++ " || { echo \"creating input file " ++ infile
                ++ "\" && dd if=/dev/random of=" ++ infile
                ++ " bs=" ++ show blockSize
                ++ " count=" ++ show blockCount
                ++ ";}"

    -- XXX this will work only on Unix systems
    runProcess_ (shell cmd)
    inHandle <- openFile infile ReadMode
    outHandle <- openFile outfile WriteMode
    href <- newIORef $ Handles inHandle outHandle

-- This is a 500MB file for text processing benchmarks.  We cannot have it in
-- the repo, therefore we use it only with DEVBUILD.
#ifdef DEVBUILD
    inText <- openFile "benchmark/text-processing/gutenberg-500.txt" ReadMode
#endif

    defaultMain
        [ bgroup "readArray"
            [ mkBench "last" href $ do
                Handles inh _ <- readIORef href
                let s = A.fromHandleArrays inh
                lc <- S.last s
                return $ case lc of
                    Nothing -> Nothing
                    Just c -> A.last c
            -- Note: this cannot be fairly compared with GNU wc -c or wc -m as
            -- wc uses lseek to just determine the file size rather than reading
            -- and counting characters.
            , mkBench "length (bytecount)" href $ do
                Handles inh _ <- readIORef href
                let s = A.fromHandleArrays inh
                S.sum (S.map A.length s)
            , mkBench "sum" href $ do
                Handles inh _ <- readIORef href
                let s = A.fromHandleArrays inh
                S.foldl' (\acc arr -> acc + A.foldl' (+) 0 arr) 0 s
            ]
        , bgroup "readStream"
            [ mkBench "last" href $ do
                Handles inh _ <- readIORef href
                S.last $ IO.fromHandle inh
            , mkBench "length (bytecount)" href $ do
                Handles inh _ <- readIORef href
                S.length $ IO.fromHandle inh
            , mkBench "sum" href $ do
                Handles inh _ <- readIORef href
                S.sum $ IO.fromHandle inh
            ]
        , bgroup "copyArray"
            [ mkBench "copy" href $ do
                Handles inh outh <- readIORef href
                let s = A.fromHandleArrays inh
                A.toHandleArrays outh s
            ]
        , bgroup "copyStream"
            [ mkBench "fromToHandle" href $ do
                Handles inh outh <- readIORef href
                IO.toHandle outh (IO.fromHandle inh)
            ]
#ifdef DEVBUILD

        , bgroup "grouping"
            [ mkBench "groupsOf 1 (toArray)" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.groupsOf fileSize (FL.toArrayN fileSize)
                                (IO.fromHandle inh)

            , mkBench "groupsOf 1" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.groupsOf 1 FL.drain (IO.fromHandle inh)
            , mkBench "groupsOf 10" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.groupsOf 10 FL.drain (IO.fromHandle inh)
            , mkBench "groupsOf 1000" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.groupsOf 1000 FL.drain (IO.fromHandle inh)
            ]

        , bgroup "splitting"
            [ bgroup "predicate"
                [ mkBenchText "splitBy \\n (line count)" inText $ do
                    (S.length $ FL.splitBy (== fromIntegral (ord '\n')) FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "splitSuffixBy \\n (line count)" inText $ do
                    (S.length $ FL.splitSuffixBy (== fromIntegral (ord '\n')) FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "wordsBy isSpace (word count)" inText $ do
                    (S.length $ FL.wordsBy (\x -> isSpace $ chr (fromIntegral x)) FL.drain $
                        IO.fromHandle inText) >>= print
                ]

            , bgroup "empty-pattern"
                [ mkBenchText "splitOn \"\"" inText $ do
                    (S.length $ FL.splitOn (A.nil) FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "splitSuffixOn \"\"" inText $ do
                    (S.length $ FL.splitSuffixOn (A.nil) FL.drain
                        $ IO.fromHandle inText) >>= print
                ]
            , bgroup "short-pattern"
                [ mkBenchText "splitOn \\n (line count)" inText $ do
                    (S.length $ FL.splitOn (A.singleton (fromIntegral (ord '\n'))) FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "splitSuffixOn \\n (line count)" inText $ do
                    (S.length $ FL.splitSuffixOn (A.singleton (fromIntegral (ord '\n'))) FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "splitOn a" inText $ do
                    (S.length $ FL.splitOn (A.fromList $ map (fromIntegral . ord) "a") FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "splitOn \\r\\n" inText $ do
                    (S.length $ FL.splitOn (A.fromList $ map (fromIntegral . ord) "\r\n") FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "splitSuffixOn \\r\\n)" inText $ do
                    (S.length $ FL.splitSuffixOn (A.fromList $ map (fromIntegral . ord) "\r\n") FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "splitOn aa" inText $ do
                    (S.length $ FL.splitOn (A.fromList $ map (fromIntegral . ord) "aa") FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "splitOn aaaa" inText $ do
                    (S.length $ FL.splitOn (A.fromList $ map (fromIntegral . ord) "aaaa") FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "splitOn abcdefgh" inText $ do
                    (S.length $ FL.splitOn (A.fromList $ map (fromIntegral . ord) "abcdefgh") FL.drain
                        $ IO.fromHandle inText) >>= print
                ]
            , bgroup "long-pattern"
                [ mkBenchText "splitOn abcdefghi" inText $ do
                    (S.length $ FL.splitOn (A.fromList $ map (fromIntegral .  ord) "abcdefghi") FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "splitOn catcatcatcatcat" inText $ do
                    (S.length $ FL.splitOn (A.fromList $ map (fromIntegral . ord) "catcatcatcatcat") FL.drain
                        $ IO.fromHandle inText) >>= print
                , mkBenchText "splitOn abc...xyz" inText $ do
                    (S.length $ FL.splitOn (A.fromList $ map (fromIntegral . ord)
                        "abcdefghijklmnopqrstuvwxyz") FL.drain
                            $ IO.fromHandle inText) >>= print
                , mkBenchText "splitSuffixOn abc...xyz" inText $ do
                    (S.length $ FL.splitSuffixOn (A.fromList $ map (fromIntegral . ord)
                        "abcdefghijklmnopqrstuvwxyz") FL.drain
                            $ IO.fromHandle inText) >>= print
                ]
            ]
#endif
        ]

    where

    mkBench :: NFData b => String -> IORef Handles -> IO b -> Benchmark
    mkBench name ref action =
        bench name $ perRunEnv (do
                (Handles inh outh) <- readIORef ref
                hClose inh
                hClose outh
                inHandle <- openFile infile ReadMode
                outHandle <- openFile outfile WriteMode
                writeIORef ref (Handles inHandle outHandle)
            )
            (\_ -> action)

#ifdef DEVBUILD
    mkBenchText name h action =
        bench name $ perRunEnv (hSeek h AbsoluteSeek 0) (\_ -> action)
#endif
