module Data.BetterIO.InStream where
	import Data.ByteString (ByteString)
	import Data.Word
	import System.IO
	import qualified Data.ByteString as BS

	class InStream a where
		inStreamRead :: a -> Integer -> IO ByteString
		inStreamReadByte :: a -> IO Word8
		inStreamReadSome :: a -> Integer -> IO ByteString

	instance InStream Handle where
		inStreamReadByte handle = (BS.hGetSome handle 1) >>= return . BS.head
		-- inStreamRead :: Handle -> Integer -> IO ByteString
		inStreamReadSome handle = (BS.hGetSome handle) . fromIntegral 
		-- inStreamRead :: Handle -> Integer -> IO ByteString
		inStreamRead handle = (BS.hGet handle) . fromIntegral

