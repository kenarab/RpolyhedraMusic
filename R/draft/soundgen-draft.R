library(RpolyhedraMusic)
#Returns the synthesized waveform as a numeric vector.

Examples

# NB: GUI for soundgen is available as a Shiny app.
# Type "soundgen_app()" to open it in default browser

#playback = c(TRUE, FALSE)[2]  # set to TRUE to play back the audio from examples
playback = TRUE
soundgen_app()

soundgen::setWavPlayer('/usr/bin/afplay')




sound = soundgen(play = playback)
# spectrogram(sound, 16000, osc = TRUE)
# playme(sound)

# Control of intonation, amplitude envelope, formants
s0 = soundgen(
  pitch = c(300, 390, 250),
  ampl = data.frame(time = c(0, 50, 300), value = c(-5, -10, 0)),
  attack = c(10, 50),
  formants = c(600, 900, 2200),
  play = playback
)

# Use the in-built collection of presets:
# names(presets)  # speakers
# names(presets$Chimpanzee)  # calls per speaker
s1 = eval(parse(text = presets$Chimpanzee$Scream_conflict))  # screaming chimp
playme(s1)
s2 = eval(parse(text = presets$F1$Scream))
# playme(s2)

s2 = eval(parse(text = presets$Cat$Howl))
playme(s2)

s2 = eval(parse(text = presets$Cat$Heat))
playme(s2)

s2 = eval(parse(text = presets$Cat$Snarl))
playme(s2)

## Not run:
# unless temperature is 0, the sound is different every time
for (i in 1:3) sound = soundgen(play = playback, temperature = .2)

# Bouts versus syllables. Compare:
sound = soundgen(formants = 'uai', repeatBout = 3, play = playback)

sound = soundgen(formants = 'aeiou', nSyl = 3, repeatBout = 3, play = playback)
sound = soundgen(formants = 'aeiou', nSyl = 3, play = playback)

sound = soundgen(formants = 'uai', nSyl = 3, play = playback)

# Intonation contours per syllable and globally:
sound = soundgen(nSyl = 5, sylLen = 200, pauseLen = 140,
                 play = playback, pitch = data.frame(
                   time = c(0, 0.65, 1), value = c(977, 1540, 826)),
                 pitchGlobal = data.frame(time = c(0, .5, 1), value = c(-6, 7, 0)))

# Subharmonics in sidebands (noisy scream)
sound = soundgen (nonlinBalance = 100, subFreq = 75, subDep = 130,
                  pitch = data.frame(
                    time = c(0, .3, .9, 1), value = c(1200, 1547, 1487, 1154)),
                  sylLen = 800,
                  play = playback, plot = TRUE)

# Jitter and mouth opening (bark, dog-like)
sound = soundgen(repeatBout = 2, sylLen = 160, pauseLen = 100,
                 nonlinBalance = 100, subFreq = 100, subDep = 60, jitterDep = 1,
                 pitch = c(559, 785, 557,321, 2123,212),
                 mouth = c(0, 0.5, 0),
                 vocalTract = 15, play = playback)

# Use nonlinRandomWalk to crease reproducible examples of sounds with
# nonlinear effects. For ex., to make a sound with no effect in the first
# third, subharmonics in the second third, and jitter in the final third of the
# total duration:
  a = c(rep(0, 100), rep(1, 100), rep(2, 100))
s = soundgen(sylLen = 800, pitch = 300, temperature = 0.001,
             subFreq = 100, subDep = 70, jitterDep = 1,
             nonlinRandomWalk = a, plot = TRUE, ylim = c(0, 4))
 playme(s)

 s = soundgen(sylLen = 800, pitch = 300*1.5, temperature = 0.003,
              subFreq = 80, subDep = 70, jitterDep = 1,
              nonlinRandomWalk = a, plot = TRUE, ylim = c(0, 4))
 playme(s)

# See the vignette on sound generation for more examples and in-depth
# explanation of the arguments to soundgen()
# Examples of code for creating human and animal vocalizations are available
# on project's homepage: http://cogsci.se/soundgen.html

## End(Not run)

