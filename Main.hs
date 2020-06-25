import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf


type Beats = Float
type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float


outputFilePath :: FilePath
outputFilePath = "output.bin"


volume :: Float
volume = 0.7

sampleRate :: Samples
sampleRate = 48000

pitchStandard :: Hz
pitchStandard = 440.0


bpm :: Beats
bpm = 120.0


beatDuration :: Seconds
beatDuration = 60.0 / bpm

  
f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0/12)) ** n


othman x = if sin x > 0 then 1.0 else -1.0

freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
  map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output
    where step = (hz * 2 * pi) / sampleRate

          attack :: [Pulse]
          attack = map (min 1.0) [0.0, 0.001 ..]

          release :: [Pulse]
          release = reverse $ take (length output) attack

          output :: [Pulse]
          output = map othman $ map (* step) [0.0 .. sampleRate * duration]


note :: Semitones -> Beats -> [Pulse]  
note n beats = freq (f n) (beats * beatDuration)


sand :: [Pulse]
sand = concat [ note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.5

              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.5

              , note 5 0.25
              , note 5 0.25
              , note 5 0.25
              , note 5 0.25
              , note 5 0.25
              , note 5 0.25
              , note 5 0.5

              , note 3 0.25
              , note 3 0.25
              , note 3 0.25
              , note 3 0.25
              , note 3 0.25
              , note 3 0.25
              , note 3 0.5

              , note (-2) 0.5
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.5

              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.5

              , note 5 0.5
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.25
              , note 0 0.5
              ]

test = concat [ note 0 1.0
              , note 0 1.0
              ]


major :: [Pulse]
major = concat [ note 0  1.0
               , note 2  1.0
               , note 3  1.0
               , note 5  1.0
               , note 7  1.0
               , note 9  1.0
               , note 11 1.0
               , note 12 2.0
               ]
  

save :: [Pulse] -> FilePath -> IO ()
save wave filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE wave


play :: [Pulse] -> IO ()
play wave = do
  save wave outputFilePath
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()


main :: IO ()
main = do
  -- play major
  -- play sand
  play test
