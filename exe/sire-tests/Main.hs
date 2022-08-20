module Main (main) where
import qualified Sire.TestExe
import Plun.JetHash (installJetHashes)
import Plun.JetImpl (installJetImpls)
import Prelude ((>>))
main = installJetHashes >> installJetImpls >> Sire.TestExe.main
