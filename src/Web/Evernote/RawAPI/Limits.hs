{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, OverloadedLists  #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances                        #-}
module Web.Evernote.RawAPI.Limits where
import Web.Evernote.TH

parseIDLFile "data/evernote-thrift/src/Limits.thrift"
