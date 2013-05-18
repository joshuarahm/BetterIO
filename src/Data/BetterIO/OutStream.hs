module Data.BetterIO.OutStream where
	import Data.ByteString (ByteString)
	import qualified Data.ByteString as BS
	import Data.Word
	import System.IO

	class OutStream a where
		outStreamWrite :: a -> ByteString -> IO ()
		outStreamWriteByte :: a -> Word8 -> IO ()

	instance OutStream Handle where
		-- outStreamWrite :: Handle -> ByteString -> IO ()
		outStreamWrite = BS.hPut 

		-- outStreamWriteByte :: Handle -> Word8 -> IO ()
		outStreamWriteByte h w = (BS.hPut h) . BS.pack $ [w]
