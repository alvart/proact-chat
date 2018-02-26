{-
  @license MIT
  Json.purs
-}

module Json
  ( parseJSON
  , stringify
  )
where

foreign import parseJSON :: forall a . String -> a

foreign import stringify :: forall a . a -> String
