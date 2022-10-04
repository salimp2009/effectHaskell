module WebAPI0 where

getReq :: [String] -> IO String
getReq []      = pure "Ok"
getReq [op, _] = 
    case op of 
      "title"   -> pure "Haskel in Depth"
      "year"    -> pure "2021"
      "rating"  -> pure "Great"
      _         -> ioError (userError "not implemented")
getReq _       = ioError (userError "Malformed Request")     





