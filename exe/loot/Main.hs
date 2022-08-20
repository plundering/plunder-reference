module Main (main) where
import qualified Loot.ReplExe
import Plun.JetHash (installJetHashes)
import Plun.JetImpl (installJetImpls)
import Prelude ((>>))
main = installJetHashes >> installJetImpls >> Loot.ReplExe.main
