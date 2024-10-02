{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson               (FromJSON, ToJSON, eitherDecode,
                                           encode)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BL
import           GHC.Generics             (Generic)
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai              (Application, requestBody,
                                           responseLBS)
import           Network.Wai.Handler.Warp (run)
import           System.IO                (writeFile)

-- Define the Payment data type
data Payment = Payment
    { id          :: Int
    , amount      :: Float
    , description :: String
    , date        :: String
    } deriving (Show, Generic)

-- Make Payment an instance of ToJSON and FromJSON
instance ToJSON Payment
instance FromJSON Payment

main :: IO ()
main = do
    putStrLn "Starting server on port 3000..."
    run 3000 app

-- Application that handles requests
app :: Application
app req respond = do
    body <- requestBody req
    let lazyBody = BL.fromStrict body  -- Convert strict ByteString to lazy ByteString
    case eitherDecode lazyBody of
        Left err -> respond $ responseLBS status400 [("Content-Type", "text/plain")] "Invalid JSON"
        Right payments -> do
            savePayments payments
            respond $ responseLBS status200 [("Content-Type", "text/plain")] "Payments saved successfully!"

-- Function to save payments to a JSON file
savePayments :: [Payment] -> IO ()
savePayments payments = do
    let jsonPayments = encode payments
    BL.writeFile "payments.json" jsonPayments
