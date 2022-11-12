{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SPDX3.Extensible
    where
import GHC.Generics (Generic)
import Data.Aeson ( ToJSON )
import Data.Data ( Typeable, Proxy(..), typeOf, typeRep, TypeRep )
import Data.Dynamic ( toDyn, Typeable, fromDynamic, Dynamic )
import qualified Data.HashMap.Strict as Map

-- ############################################################################
-- ##  Extension  #############################################################
-- ############################################################################

class (Extensible a) => Extension a where
    acceptsParent :: a -> Dynamic -> Bool
    getExtensionName :: a -> String

data PackedExtension where
    PackedExtension :: (Extension a) => a -> PackedExtension
unpackExtension :: (Typeable e, Extension e) => PackedExtension -> Maybe e
unpackExtension (PackedExtension e) = (fromDynamic . toDyn) e
type ExtensionMap = Map.HashMap TypeRep PackedExtension

class (ToJSON a, Typeable a) => Extensible a where
    getExtensionsMap :: a -> ExtensionMap
    setExtensionsMap :: ExtensionMap -> a -> a
    addExtension :: Extension e => e -> a -> a
    addExtension e a = let
            extensionsMap = getExtensionsMap a
        in if acceptsParent e (toDyn a)
           then setExtensionsMap (Map.insert (typeOf e) (PackedExtension e) extensionsMap) a
           else error "this extension is not allowed to be added to this parent. I know it should be covered by the type system, yes."
    getExtension ::  forall e . (Typeable e, Extension e) => a -> Maybe e
    getExtension a = let
            want = typeRep (Proxy :: Proxy e)
            findFirstMaybe (e@(Just _):_) = e
            findFirstMaybe (_:es) = findFirstMaybe es
            findFirstMaybe [] = Nothing
            getExtensionFromDynamic :: PackedExtension -> Maybe e
            getExtensionFromDynamic dyn = undefined
        in case Map.lookup want (getExtensionsMap a) of
            Just dyn -> unpackExtension dyn
            _        -> findFirstMaybe (map getExtensionFromDynamic (Map.elems (getExtensionsMap a)))