module WebAPI0 where

-- | getReq is the main object in this WebApi example
-- that mimic a client request and a Haskell web service
-- that gets and check the request from a data base and
-- send repsonse to client
-- this module represents the getting the request and checking 

-- >>> getReq ["title", "324534"]
-- "Haskel in Depth"

-- >>> getReq ["year", "324534"]
-- "2021"
getReq :: [String] -> IO String
getReq []      = pure "OK"
getReq [op, _] = 
    case op of 
      "title"   -> pure "Haskel in Depth"
      "year"    -> pure "2021"
      "rating"  -> pure "Great"
      _         -> ioError (userError "not implemented")
getReq _       = ioError (userError "Malformed Request") 

checkReq :: IO()
checkReq = do
  b <- getReq []
  y <- getReq ["year", "7548"]
  putStrLn (if b=="OK" && y == "2021"
            then "OK"
            else "Wrong Answer"
           )



