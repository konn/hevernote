{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures            #-}
{-# LANGUAGE OverloadedLists, TemplateHaskell, UndecidableInstances       #-}
module Web.Evernote.RawAPI.UserStore where
import Web.Evernote.RawAPI.Errors as Errors
import Web.Evernote.RawAPI.Types  as Types
import Web.Evernote.TH

parseIDLFile "data/evernote-thrift/src/UserStore.thrift"
