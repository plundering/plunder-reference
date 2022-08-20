module Main (main) where
import qualified Loot.TestExe
import Plun.JetHash (installJetHashes)
import Plun.JetImpl (installJetImpls)
import Prelude ((>>))
main = installJetHashes >> installJetImpls >> Loot.TestExe.main
