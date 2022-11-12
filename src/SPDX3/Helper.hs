{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module SPDX3.Helper where
import           SPDX3.Model
import           SPDX3.SPDXID

getElements :: SPDX a -> [SPDX ()]
getElements =
  let
    getImplicitElements' :: SPDX a -> [SPDX ()]
    getImplicitElements' (Pack e) = getImplicitElements e
    getImplicitElements' (Collection _ (CollectionProperties es res _ _)) =
      concatMap getImplicitElements es ++ concatMap getImplicitElements res
    getImplicitElements' (Relationship _ (RelationshipProperties _ from to _))
      = concatMap getImplicitElements (from : to)
    getImplicitElements' (Annotation _ (AnnotationProperties _ subject)) =
      concatMap getImplicitElements [subject]
    getImplicitElements' _ = []
    getImplicitElements :: forall a . SPDX a -> [SPDX ()]
    getImplicitElements e =
      getImplicitElements' e ++ maybe [] getImplicitElements (getParent e)
  in \e -> pack e : getImplicitElements e

getSPDXIDs :: SPDX a -> [SPDXID]
getSPDXIDs = map getSPDXID . getElements
