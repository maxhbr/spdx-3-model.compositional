module SPDX3.Monad
    where
import           Control.Monad.Except         (ExceptT, runExceptT)
import           Control.Monad.Identity       (Identity)
import           Control.Monad.Reader
import           Data.Time.Format.ISO8601
import           SPDX3.Model


-- ############################################################################
-- ##  monad  #################################################################
-- ############################################################################

type SPDX_M a = ExceptT String (ReaderT CreationInfo Identity) a
    -- ReaderT CreationInfo (Except a) a

runSPDX :: CreationInfo -> SPDX_M a -> Either String a
runSPDX ci = (`runReader` ci) . runExceptT

ref :: SPDXID -> SPDX_M (SPDX ())
ref = return . Ref

packM :: Maybe SPDXID ->  SPDX_M (SPDXID -> SPDX a) -> SPDX_M (SPDX ())
packM (Just spdxid) = fmap (pack . (\f -> f spdxid))
packM Nothing = fmap (pack . setSPDXIDFromContent)

element' :: Element -> SPDX_M (SPDXID -> SPDX Element)
element' ep = do
    creationInformation <- ask
    return (\spdxid -> Element spdxid creationInformation ep)
element :: Maybe SPDXID -> Element -> SPDX_M (SPDX ())
element spdxid ep = packM spdxid $ element' ep

artifact' :: Artifact -> Element -> SPDX_M (SPDXID -> SPDX Artifact)
artifact' ap ep = do
    efun <- element' ep
    return ((`Artifact` ap) . efun )
artifact :: Maybe SPDXID -> Artifact -> Element -> SPDX_M (SPDX ())
artifact spdxid ap ep = packM spdxid $ artifact' ap ep

collection' :: Collection -> Element -> SPDX_M (SPDXID -> SPDX Collection)
collection' cp ep = do
    efun <- element' ep
    return ((`Collection` cp) . efun )
collection :: Maybe SPDXID -> Collection -> Element -> SPDX_M (SPDX ())
collection spdxid cp ep = packM spdxid $ collection' cp ep

bundle' :: Bundle -> Collection -> Element -> SPDX_M (SPDXID -> SPDX Bundle)
bundle' bp cp ep = do
    cfun <- collection' cp ep
    return ((`Bundle` bp) . cfun )
bundle :: Maybe SPDXID -> Bundle -> Collection -> Element -> SPDX_M (SPDX ())
bundle spdxid bp cp ep = packM spdxid $ bundle' bp cp ep

spdxDocument' :: Bundle -> Collection -> Element -> SPDX_M (SPDXID -> SPDX SpdxDocument)
spdxDocument' _ _ (ElementProperties{_elementName = Nothing} ) = error "Name for spdxDocument is required"
spdxDocument' bp cp ep = do
    bfun <- bundle' bp cp ep
    return (SpdxDocument . bfun)
spdxDocument :: Maybe SPDXID -> Bundle -> Collection -> Element -> SPDX_M (SPDX ())
spdxDocument spdxid bp cp ep = packM spdxid $ spdxDocument' bp cp ep

annotation' :: Annotation -> Element -> SPDX_M (SPDXID -> SPDX Annotation)
annotation' ap ep = do
    efun <- element' ep
    return ((`Annotation` ap) . efun )
annotation :: Maybe SPDXID -> Annotation -> Element -> SPDX_M (SPDX ())
annotation spdxid ap ep = packM spdxid $ annotation' ap ep

-- ############################################################################
-- ##  example  ###############################################################
-- ############################################################################

mkExample' :: IO (Either String (SPDX ()))
mkExample' = do
    let actors = [Actor (Just "Some Actor") (Just PERSON), Actor (Just "This Tool") (Just TOOL)]
    created <- iso8601ParseM "2022-11-13T13:14:36.324980945Z"
    let creationInfo = mkCreationInfo actors created
    return . runSPDX creationInfo $ do

        r0 <- ref "urn:spdx:Ref0"
        r1 <- ref "urn:spdx:Ref1"
        r2 <- ref "urn:spdx:Ref2"

        a0 <- artifact (Just "urn:spdx:Artifact0") def def

        an0 <- annotation (Just "urn:spdx:Annotation0") (AnnotationProperties "some Annotation" r2) def
        c1 <- bundle Nothing def def{_collectionElements = [an0]} def

        let elements = [r0, r1, r2, a0, c1]

        spdxDocument (Just "urn:spdx:Collection0" ) def def{_collectionElements = elements} def{_elementName = Just "The Document"}

mkExample :: IO (SPDX ())
mkExample = do
    (Right result) <- mkExample'
    return result

