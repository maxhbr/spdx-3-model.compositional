module SPDX3.Monad
    where
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict    as Map
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           GHC.Word               (Word8)
import           Data.Time.Format.ISO8601
import           SPDX3.Model.CreationInfo
import           SPDX3.Model.RelationshipType
import           SPDX3.Model.SPDXID
import           SPDX3.Model
import SPDX3.Model (Bundle(BundleProperties))
import Control.Monad.Except (Except, ExceptT, runExceptT)
import Control.Monad.Identity (Identity)


-- ############################################################################
-- ##  monad  #################################################################
-- ############################################################################

type SPDX_M a = ExceptT String (ReaderT CreationInfo Identity) a
    -- ReaderT CreationInfo (Except a) a

runSPDX :: CreationInfo -> SPDX_M a -> Either String a
runSPDX ci = (`runReader` ci) . runExceptT

ref :: SPDXID -> SPDX_M (SPDX ())
ref = return . Ref

artifact' :: SPDXID -> String -> SPDX_M (SPDX Artifact)
artifact' i name = do
    ci <- ask
    let ie = Element $ elementFromName i ci name
    return (Artifact ie (ArtifactProperties []))
artifact :: SPDXID -> String -> SPDX_M (SPDX ())
artifact i name = pack <$> artifact' i name

collection' :: SPDXID -> SPDX_M [SPDX ()] -> SPDX_M (SPDX Collection)
collection' i es = do
    ci <- ask
    let ie = Element $ emptyElement i ci
    Collection ie . (\es -> CollectionProperties es [] mempty mempty) <$> es
collection :: SPDXID -> SPDX_M [SPDX ()] -> SPDX_M (SPDX ())
collection i es = pack <$> collection' i es

spdxDocument' :: String -> SPDX_M (SPDX Collection) -> SPDX_M (SPDX SpdxDocument)
spdxDocument' name collection = SpdxDocument . (`Bundle` BundleProperties Nothing) <$> collection
spdxDocument :: String -> SPDX_M (SPDX Collection) -> SPDX_M (SPDX ())
spdxDocument name collection = pack <$> spdxDocument' name collection

annotation' :: SPDXID -> String -> SPDX_M (SPDX ()) -> SPDX_M (SPDX Annotation)
annotation' i stmnt subject = do
    ci <- ask
    let ie = Element $ emptyElement i ci
    Annotation ie . AnnotationProperties stmnt <$> subject
annotation :: SPDXID -> String -> SPDX_M (SPDX ()) -> SPDX_M (SPDX ())
annotation i stmnt subject = pack <$> annotation' i stmnt subject

-- ############################################################################
-- ##  example  ###############################################################
-- ############################################################################

mkExample :: IO (SPDX ())
mkExample = do
    let actors = [Actor (Just "Some Actor") (Just PERSON), Actor (Just "This Tool") (Just TOOL)]
    created <- iso8601ParseM "2022-11-13T13:14:36.324980945Z"
    let creationInfo = mkCreationInfo actors created
    let (Right result) = runSPDX creationInfo $
                        spdxDocument "The Document" $
                        collection' "urn:spdx:Collection0" $ do
                            r0 <- ref "urn:spdx:Ref0"
                            r1 <- ref "urn:spdx:Ref1"
                            r2 <- ref "urn:spdx:Ref2"
                            a0 <- artifact "urn:spdx:Artifact0" "Artifact0"
                            c1 <- collection "urn:spdx:Collection1" $ do
                                an0 <- annotation "urn:spdx:Annotation0" "Some Annotation" $ do
                                    ref "urn:spdx:Artifact0"
                                return [an0]
                            return [r0,r1,r2,a0,c1]
    return result

