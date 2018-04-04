{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, TemplateHaskell  #-}
{-# LANGUAGE UndecidableInstances                                         #-}
module Web.Evernote.RawAPI.Types where
import Web.Evernote.TH

parseIDLFile "data/evernote-thrift/src/Types.thrift"
