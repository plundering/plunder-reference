module Main (main) where
import qualified Server.ConsoleExe
import Plun.JetHash (installJetHashes)
import Plun.JetImpl (installJetImpls)
import Prelude ((>>))
main = installJetHashes >> installJetImpls >> Server.ConsoleExe.main
