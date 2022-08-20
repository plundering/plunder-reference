module Main (main) where
import qualified Sire.ReplExe
import Plun.JetHash (installJetHashes)
import Plun.JetImpl (installJetImpls)
import Prelude ((>>))
main = installJetHashes >> installJetImpls >> Sire.ReplExe.main
