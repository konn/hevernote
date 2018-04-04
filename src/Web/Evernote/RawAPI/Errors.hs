{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, TemplateHaskell  #-}
{-# LANGUAGE UndecidableInstances                                         #-}
module Web.Evernote.RawAPI.Errors where
import qualified Web.Evernote.RawAPI.Types as Types
import           Web.Evernote.TH

parseIDLFile "data/evernote-thrift/src/Errors.thrift"
