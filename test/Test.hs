module Main where
	import Data.ByteString (ByteString)
	import qualified Data.ByteString as BS
	import Data.BetterIO.InStream
	import Data.BetterIO.OutStream
	import Data.BetterIO.PipedStream
	import Data.String

	import System.IO
	import Control.Concurrent

	readThread :: PipedStream -> IO ()
	readThread stream = do
		bs <- inStreamReadSome stream 5
		putStrLn $ (show bs)
		readThread stream

	writeThread :: PipedStream -> IO ()
	writeThread stream =
		let writeChunks :: ByteString -> IO () ; writeChunks bs =
			let (chunk,rest) = BS.splitAt 7 bs in do
				outStreamWrite stream chunk
				if (not $ BS.null rest) then writeChunks rest else return () in do
		str <- getLine
		writeChunks (fromString str)
		writeThread stream

	main = do
		ps <- newPipedStream
		_ <- forkIO $ readThread ps
		writeThread ps
