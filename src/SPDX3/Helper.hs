{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module SPDX3.Helper where
import           SPDX3.Model
import           SPDX3.SPDXID

getElements :: SPDX a -> [SPDX ()]
getElements e =
  let
    getImplicitElements' :: SPDX a -> [SPDX ()]
    getImplicitElements' (Pack e) = getImplicitElements e
    getImplicitElements' e@(Collection _ (CollectionProperties es res _ _)) =
      concatMap getImplicitElements es ++ concatMap getImplicitElements res
    getImplicitElements' e@(Relationship _ (RelationshipProperties _ from to _))
      = concatMap getImplicitElements (from : to)
    getImplicitElements' e@(Annotation _ (AnnotationProperties _ subject)) =
      concatMap getImplicitElements [subject]
    getImplicitElements' _ = []
    getImplicitElements :: forall a . SPDX a -> [SPDX ()]
    getImplicitElements e =
      getImplicitElements' e ++ maybe [] getImplicitElements (getParent e)
  in
    pack e : getImplicitElements e

getSPDXIDs :: SPDX a -> [SPDXID]
getSPDXIDs = map getSPDXID . getElements