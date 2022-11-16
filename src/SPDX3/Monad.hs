module SPDX3.Monad
    where
import           Control.Monad.Except   (ExceptT, runExceptT)
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader
import           GHC.Generics           (Datatype (packageName))
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
packM Nothing       = fmap (pack . setSPDXIDFromContent)

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

relationship' :: Relationship -> Element -> SPDX_M (SPDXID -> SPDX Relationship)
relationship' rp ep = do
    efun <- element' ep
    return ((`Relationship` rp) . efun )
relationship :: Maybe SPDXID -> Relationship -> Element -> SPDX_M (SPDX ())
relationship spdxid cp ep = packM spdxid $ relationship' cp ep

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

package' :: Package -> Artifact -> Element -> SPDX_M (SPDXID -> SPDX Package)
package' pp ap ep = do
    afun <- artifact' ap ep
    return ((`Package` pp) . afun )
package :: Maybe SPDXID -> Package -> Artifact -> Element -> SPDX_M (SPDX ())
package spdxid pp ap ep = packM spdxid $ package' pp ap ep

file' :: File -> Artifact -> Element -> SPDX_M (SPDXID -> SPDX File)
file' fp ap ep = do
    afun <- artifact' ap ep
    return ((`File` fp) . afun )
file :: Maybe SPDXID -> File -> Artifact -> Element -> SPDX_M (SPDX ())
file spdxid fp ap ep = packM spdxid $ file' fp ap ep

snippet' :: Snippet -> Artifact -> Element -> SPDX_M (SPDXID -> SPDX Snippet)
snippet' sp ap ep = do
    afun <- artifact' ap ep
    return ((`Snippet` sp) . afun )
snippet :: Maybe SPDXID -> Snippet -> Artifact -> Element -> SPDX_M (SPDX ())
snippet spdxid sp ap ep = packM spdxid $ snippet' sp ap ep
