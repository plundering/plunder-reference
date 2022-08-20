{- | This is a hack to avoid long compile times when changing jet hashes.

    TODO The `unsafePerformIO` hack could be avoided through careful
    engineering.

    TODO Maybe we should even read these from a file?
-}

module Plun.JetHash (jetHashes, installJetHashes) where

import ClassyPrelude
import Plun.Print
import Plun.Jets

--------------------------------------------------------------------------------

{-
    Call this immediatly on executable startup.
-}
installJetHashes :: IO ()
installJetHashes =
    writeIORef vJetHash jetHashes

jetHashes :: Map Text ByteString
jetHashes
    = mapFromList
    $ map (\(t,b::Text) -> (t, decodeBtc b))
    [ ( "dec"         , "AVQCC5kQN5ZXVaVFDNzTmwM8qx49Rkhefb2hQRX13ZDL" )
    , ( "add"         , "DmQSboknWfMEi5VSrqzM5AkXNcmEJAagtGum48t9D5yY" )
    , ( "mul"         , "DLZ7asf6Z8JKVJh7XYdPE2vvchwLi8SYDqg1KdmRPGWM" )
    , ( "sub"         , "B6MvfHoH5QAQRCkFB3aF2GkmWZY5FssjmKV4VmDyEHjY" )
    , ( "bex"         , "3r98Xw8DcPxaa3fG5UVGPbPN7Bz4ZDYXQ98d1KcNLzhM" )
    , ( "lte"         , "Cjd1agEzdQheY5yWYBNEqhvftas2LpxWjByEeLZjukcx" )
    , ( "lth"         , "EAY3teuKWH2eoebjSjxJy6PzTnYAgttf9kiXsi7JgMh5" )
    , ( "div"         , "9MxPwrQykhLZWdsY4MJjCT9u7xff1TX4RZMTqfQAbrgY" )
    , ( "mod"         , "6dVRAXWbKRo6t98G4g7spdcCJaEox976uBs3muQAcmYP" )
    , ( "aeq"         , "J4zPLvTafdRuJNvD7h3q9ZukAxVrnRryU2YkpN1L2uuw" )
    , ( "lsh"         , "GCSFeHBqSCgm2RccZihForF5LaXWwvNrES4mC78qxStV" )
    , ( "rsh"         , "BgYuMkPS6Jx8yQ1Z2feWLuhceJKD2UkHiWFRcyDViLHo" )
    , ( "mix"         , "2sC3UyN5QHidiUYZVMC9tsrwxT6BgBSKBZnDRqTqXiWW" )
    , ( "dis"         , "5QD2dHRSywxcyb65iPchURuxv7KafeWf6D8dQbNvkFnr" )
    , ( "con"         , "3psgaoUW83hFnzvZeqVtpSaCUw8SdjFxSatMb3qzNAUF" )
    , ( "if"          , "3by2HMZG752GK3emojoLEdeJDbemCWnPC7tVbjQwhCrF" )
    , ( "ifNot"       , "9ZTWHAx7G5YKyq4HNa5anatbE2tySSj6SCyyFHd3VDGo" )
    , ( "eql"         , "2YwfJETAb9r2DHeSh7asUZphPFL1GQdx7D4NCL3oajYa" )
    , ( "trk"         , "DCwrK91sjJddiSyrP7ZMy6Lyu6rpf2tYqf7eqsLcfMhs" )
    , ( "die"         , "3Gojkov63gvagvD5ecU8GC8MtcStBu2N4MLm1X9sK7pA" )
    , ( "idx"         , "5xfxHseVdGuS2PMTf83tinbU2iUkvbPjansK1UEuih3H" )
    , ( "get"         , "CAhGhuL5UJSipHbymKtLonKRB626L3KnqeWZP98XMWdu" )
    , ( "len"         , "9Yd7MqRqG5kpSwXzezXzhWuDQkiHFKr9FqDFKWmiV33D" )
    , ( "weld"        , "FLHy75pC3QxsJxAFAJYXoxFKQGabCprUPvn6azxk1yPi" )
    , ( "map"         , "EzV5LHcu4k5nVTBCpfawni7YS3o3RP5uq41cfJ5fq8Cb" )
    , ( "put"         , "E5xDBYdpXrMNk4FsV592GsAvhE3dbeetENMDAnLfJedP" )
    , ( "mut"         , "4tndperaKWAu2qwH1EHmQdSocaUG4jXEYMWQd5hkCnFR" )
    , ( "take"        , "DN92ih5w62cY4ZiHCZoEnbqW6ddufSqnVAffXWsqppYv" )
    , ( "drop"        , "XNFLgKsLLuH11oeSBHCMdfFj93JFRizqQVTZeAucmwT"  )
    , ( "cat"         , "9Aqbib4mWbMhruaMmWGw6k8BZz1No6Y14zSa3uY8E1aZ" )
    , ( "isBar"       , "AJjj3txV5xMGYzJEQnzdw1yFBe4EducU9C1a3h12mnaA" )
    , ( "barIdx"      , "FcAUzAXH2ThrXMytb39CyAC2WQaHGq3kR7JY6MW6LTaN" )
    , ( "barWeld"     , "3osGqtX6kQWtG9v1YYEVxgR2oyjzJ8PKxpu6XaGFsp49" )
    , ( "barCat"      , "9PcJS5agnsedvsYNrMDEqBaKyvp258iRWTCXbgHni54J" )
    , ( "barFlat"     , "FBR7MuHwaHCvj3pYn7ns8cKk2QXUmTJsNDEU5CV8UUXB" )
    , ( "w32"         , "9SMRymqcMwJNyDh97Xw5ShCjsxKjpnLBFGr3qL5da5Wo" )
    , ( "add32"       , "BGrknMcAGpawVhAeDYiHWKCFMfNsMd7kxhg33zCqH9Db" )
    , ( "mul32"       , "FozGuX6GjVB7ft3easWXDNotxWsYLiDnjAx5yuMb5btf" )
    , ( "div32"       , "EiLoBsvtSpZ4s3MXozL1XgVGzxs9YPfJWYZHYy3NziNM" )
    , ( "and32"       , "63JCSviU9osUE6xVTu7hBcU4tmEWBVnXDmWHrEGsz81V" )
    , ( "or32"        , "81jmdqZ1NFo6dghjVpPhSBqRmt5bqQzE8mXmSR3a6YpZ" )
    , ( "xor32"       , "3R8hVWf8pMzHaQ7Pk74RwduzAJ2BrVDh6hSPFBKcJZrq" )

    , ( "lsh32"       , "HthoQpyK4MHGShWSLd2AkovmqDPBPHtb4FLBU2cvRbv1" )
    , ( "rsh32"       , "BmiWtCBQ5beY637SNawiM84MfA1AWAebP377Y7DWGt7P" )
    , ( "sub32"       , "DuF8ChNnZM4yBJHhzGByes8d28AEbeVnVuPjdfkyJCUn" )
    , ( "ror32"       , "6YkQk6mvDoQTpdA39tTZcH4ZugfCyEHJyxymiKsMg8Br" )
    , ( "rol32"       , "7PVT2nj15XsYWM5txxjVE6T4C1pLubKs5nnJormtSCem" )
    , ( "cordFromRow" , "GmQfVeR2uoXgQqGJgPrABqqNN1tB29VQZZxDQy3cur5Q" )
    ]
