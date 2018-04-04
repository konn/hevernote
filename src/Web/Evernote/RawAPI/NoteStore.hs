{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures            #-}
{-# LANGUAGE OverloadedLists, TemplateHaskell, UndecidableInstances       #-}
module Web.Evernote.RawAPI.NoteStore where
import Web.Evernote.RawAPI.Errors    as Errors
import Web.Evernote.RawAPI.Types     as Types
import Web.Evernote.RawAPI.UserStore as UserStore
import Web.Evernote.TH

parseIDLFile "data/evernote-thrift/src/NoteStore.thrift"
