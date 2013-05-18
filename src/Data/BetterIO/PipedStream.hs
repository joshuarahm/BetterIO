{-
 A module that defines a piped stream which can be written to
 and read from. The stream can be read exactly like other instances
 of the InStream class
-}
module Data.BetterIO.PipedStream (inStreamRead,
	inStreamReadSome, inStreamReadByte, outStreamWrite,
	outStreamWriteByte, newPipedStream )
where
	import Data.ByteString (ByteString)
	import qualified Data.ByteString as BS
	import Data.BetterIO.InStream
	import Data.BetterIO.OutStream
	import Control.Concurrent.STM.TMVar
	import Control.Concurrent.STM.TChan
	import Control.Monad.STM
	import System.IO

	{- The piped stream allows for transparent reading
		and writing from a pipe -}
	data PipedStream = PipedStream {
		-- a queue of byte strings ready to be read
		queue :: TChan ByteString,

		-- the byte string that is currently being manipulated
		deck :: TMVar ByteString
	}

	-- get the length of a byte string and convert it
	bslength2 :: (Integral a) => ByteString -> a
	bslength2 = fromIntegral . BS.length

	-- If the on deck mvar is empty, then pull the next string from
	-- the channel and put it on deck. Returns true if there is no
	-- more data left, false otherwise
	pipedStreamSift :: PipedStream -> STM Bool
	pipedStreamSift (PipedStream q d) = do
		-- check to see if the mvar is empty
		ismvarempty <- isEmptyTMVar do
		
		-- if not empty, return false
		if (not ismvarempty) then return False
		else do
			-- the on-deck mvar is empty
			-- check to see if the channel has data
			ischanempty <- isEmptyTChan q
			-- if the channel is empty there is no more data to be read
			if ischanempty then return True
			else do
				-- read the channel and put the value in the mvar
				bs <- readTChan q
				putTMVar d bs
				return False

	-- returns (if eof, bytes left, the bytes read)
	pipedStreamPullN :: Bool -> PipedStream -> Integer -> STM (Bool, Integer, ByteString)
	pipedStreamPullN block stream@(PipedStream q d) n = do
		isemptymvar <- isEmptyTMVar d
		if isemptymvar && (not block) then return (True, n, BS.empty) else do
			bs <- takeTMVar d
			let m = min n (bslength2 bs)
			let (ret, rest) = BS.splitAt (fromIntegral m) bs
			if BS.null rest then do
				eof <- pipedStreamSift stream
				return (eof, n - m, ret)
			else do
				putTMVar d rest
				return (False, n - m, ret)

	pipedStreamReads :: Bool -> PipedStream -> Integer -> IO ByteString
	pipedStreamReads blk stream n =
		let readit :: Bool -> Integer -> IO [ByteString] ; readit block n = do
			(atomically $ pipedStreamPullN block stream n) >>= (\(eof, left, bs) ->
				if left == 0 || (eof && (not blk)) then return $ [bs] else
					readit blk left >>= return . (bs:) )
		in
		readit True n >>= return . BS.concat

	pipedStreamPut :: PipedStream -> ByteString -> STM ()
	pipedStreamPut stream@(PipedStream q d) bs = do
		writeTChan q bs
		pipedStreamSift stream
		return ()
	
	newPipedStream :: IO PipedStream
	newPipedStream = do
		q <- newTChanIO
		d <- newEmptyTMVarIO
		return $ PipedStream q d

	instance InStream PipedStream where
		inStreamRead = pipedStreamReads True
		inStreamReadSome = pipedStreamReads False
		inStreamReadByte stream = (inStreamReadSome stream 1) >>= return . BS.head

	instance OutStream PipedStream where
		outStreamWrite s b = atomically $ pipedStreamPut s b
		outStreamWriteByte stream w = atomically (pipedStreamPut stream . BS.pack $ [w])
			
			

