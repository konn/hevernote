{-# LANGUAGE OverloadedStrings, PatternGuards, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, TupleSections                    #-}
module Web.Evernote.TH (interpretIDL, parseIDLFile) where
import           Control.Exception                  (Exception)
import qualified Data.ByteString                    as BS
import           Data.Int
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           Data.Maybe                         (fromMaybe)
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Typeable                      (Typeable)
import           GHC.Generics                       (Generic)
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift           (Lift (..))
import           Language.Haskell.TH.Name.CamelCase (ConName (..),
                                                     conCamelcaseName,
                                                     varCamelcaseName, varName)
import qualified Language.Thrift.AST                as T
import qualified Language.Thrift.Parser             as T
import qualified Pinch                              as P

renderConstValue :: T.ConstValue a -> ExpQ
renderConstValue (T.ConstInt i _) = lift i
renderConstValue (T.ConstFloat f _) = lift f
renderConstValue (T.ConstLiteral l _) = [| T.pack $(lift $ T.unpack l) |]
renderConstValue (T.ConstIdentifier i _) = varE $ varName $ varCamelcaseName $ T.unpack i
renderConstValue (T.ConstList l _) = listE $ map renderConstValue l
renderConstValue (T.ConstMap lst _) =
  [| M.fromList $(listE $ map (\(a,b) -> tupE [renderConstValue a, renderConstValue b]) lst) |]

renderTypeReference :: T.TypeReference a -> TypeQ
renderTypeReference (T.DefinedType t _) = conT $ mkName $ T.unpack t
renderTypeReference (T.StringType _ _) = [t| Text |]
renderTypeReference (T.BinaryType _ _) = [t| BS.ByteString |]
renderTypeReference (T.BoolType _ _) = [t| Bool |]
renderTypeReference (T.ByteType _ _) = [t| Int8 |]
renderTypeReference (T.I16Type _ _) = [t| Int16 |]
renderTypeReference (T.I32Type _ _) = [t| Int32 |]
renderTypeReference (T.I64Type _ _) = [t| Int64 |]
renderTypeReference (T.DoubleType _ _) = [t| Double |]
renderTypeReference (T.MapType k v _ _) =
    [t| Map $(renderTypeReference k) $(renderTypeReference v) |]
renderTypeReference (T.SetType i _ _) = [t| Set $(renderTypeReference i) |]
renderTypeReference (T.ListType i _ _) = [t| [$(renderTypeReference i)] |]
renderTypeReference (T.SListType _ _) = error "SList is deprecated and hence unsupported"

strictness :: Bang
strictness = Bang SourceUnpack SourceStrict

unpackStrict :: TypeQ -> BangTypeQ
unpackStrict ty = (strictness, ) <$> ty

renderTypedef :: T.Typedef a -> DecsQ
renderTypedef T.Typedef{..} =
  let name = typeName typedefName
      ty = renderTypeReference typedefTargetType
      body = normalC name [(Bang NoSourceUnpackedness NoSourceStrictness,) <$> ty]
  in pure <$> newtypeD (return []) name [] Nothing body derivs

derivs :: [DerivClauseQ]
derivs =
  let cls  = [ [t| P.Pinchable |], [t| Show |], [t| Eq |], [t| Ord |]
             ]
  in [ derivClause (Just NewtypeStrategy) cls
     , derivClause (Just StockStrategy) [[t| Generic |]]]

typeName :: Text -> Name
typeName = conName . conCamelcaseName  . T.unpack

fieldType :: T.Field a -> TypeQ
fieldType T.Field{..} =
  let typ0 = renderTypeReference fieldValueType
      req t = case fromMaybe T.Required fieldRequiredness of
                T.Required -> t
                T.Optional -> [t| Maybe $t |]
      fld t =
        case fieldIdentifier of
          Nothing -> t
          Just i  -> [t| P.Field $(litT $ numTyLit i) $t |]
  in fld $ req typ0


strDeriv :: [DerivClauseQ]
strDeriv = [ derivClause (Just StockStrategy) [[t| Generic |], [t| Show |], [t| Eq |], [t| Ord |] ]
           , derivClause (Just AnyclassStrategy) [ [t| P.Pinchable |] ]
           ]

renderStruct :: T.Struct a -> DecsQ
renderStruct T.Struct{T.structKind = T.UnionKind, ..} =
  let name = typeName structName
      cons = map (fieldToCon $ Just structName) structFields
  in pure <$> dataD (return []) name [] Nothing cons strDeriv
renderStruct T.Struct{..} =
  let derivs'
        | T.ExceptionKind <- structKind =
            strDeriv ++ [ derivClause (Just StockStrategy) [ [t| Typeable |] ]
                        , derivClause (Just AnyclassStrategy) [ [t| Exception |] ] ]
        | otherwise = strDeriv
      name = conName $ conCamelcaseName $ T.unpack structName
      con = recC name $ map (fieldToRecField structName) structFields
  in pure <$> dataD (return []) name [] Nothing [con] derivs'

fieldToCon :: Maybe T.Text -> T.Field a -> ConQ
fieldToCon mpre f@T.Field{..} =
  let typ = fieldType f
      cName = conName $ conCamelcaseName $ T.unpack $
              maybe id (\t -> T.append (T.concat [t, "_"])) mpre
              fieldName
  in normalC cName [unpackStrict typ]

fieldToRecField :: Text -> T.Field a -> VarBangTypeQ
fieldToRecField cname f@T.Field{..} =
  let fPrefix = nameBase $ varName $ varCamelcaseName $ T.unpack cname
      fName = mkName $ fPrefix ++ nameBase (conName $ conCamelcaseName $ T.unpack fieldName)
  in (fName, strictness , ) <$> fieldType f

renderConst :: T.Const a -> DecsQ
renderConst T.Const{..} =
  let cName = varName $ varCamelcaseName $ T.unpack constName
      ty    = renderTypeReference constValueType
  in sequence [ sigD cName ty
              , funD cName [clause [] (normalB (renderConstValue constValue)) [] ]
              ]

renderType :: T.Type a -> DecsQ
renderType (T.TypedefType   t) = renderTypedef t
renderType (T.EnumType      t) = renderEnum t
renderType (T.StructType    t) = renderStruct t
renderType T.SenumType{}       = error "SenumType is unsupported"

renderEnum :: T.Enum a -> DecsQ
renderEnum T.Enum{..} =
  let cons = map buildCons enumValues
  in pure <$> dataD (return []) (typeName enumName) [] Nothing cons strDeriv
  where
    buildCons T.EnumDef{..} =
      let cName0 = nameBase $ conName $ conCamelcaseName $ T.unpack enumDefName
          cName = conName $ conCamelcaseName $ T.unpack enumName ++ cName0
          eTy = [ maybe [t| P.Void |]  (\ i -> [t| P.Enumeration $(litT $ numTyLit i) |]) enumDefValue ]
      in normalC cName $ map (fmap (Bang NoSourceUnpackedness NoSourceStrictness,)) eTy

renderDefinition :: T.Definition a -> DecsQ
renderDefinition (T.ConstDefinition   c) = renderConst   c
renderDefinition (T.TypeDefinition    t) = renderType    t
renderDefinition (T.ServiceDefinition t) = renderService    t

renderFunction :: Name -> T.Function a -> ConQ
renderFunction tyName T.Function{..} =
  let name = typeName functionName
      tyCon = conT tyName
      args = map (fieldToRecField functionName) functionParameters
      retTy = maybe [t| () |] renderTypeReference functionReturnType
      excs = promotedListT $ maybe [] (map (renderTypeReference . T.fieldValueType)) functionExceptions
  in recGadtC [name] args [t| $(tyCon) $(excs) $(retTy) |]

promotedListT :: [TypeQ] -> TypeQ
promotedListT = foldr (appT . appT promotedConsT) promotedNilT

renderService :: T.Service a -> DecsQ
renderService T.Service{serviceExtends = Nothing, ..} = do
  es <- newName "excs"
  a <- newName "a"
  let name = typeName serviceName
      ops  = map (renderFunction name) serviceFunctions
  pure <$> dataD (return []) name [KindedTV es (AppT ListT StarT), PlainTV a] Nothing ops []
renderService _ = error  "Currently, extending service is unsupported"

interpretIDL :: T.Program a -> DecsQ
interpretIDL (T.Program _ defs) =
  concat <$> mapM renderDefinition defs

parseIDLFile :: FilePath -> DecsQ
parseIDLFile fp = either (fail . show) interpretIDL =<< runIO (T.parseFromFile  fp)
