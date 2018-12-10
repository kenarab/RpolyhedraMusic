nrow(polyhedra.7.faces)
library(rgl)
library(Rpolyhedra)

# 1.  Obtain 5 regular solids
all.polyhedra <- getAvailablePolyhedra()

polyhedra.7.faces <- all.polyhedra[all.polyhedra$faces==7,]

# 2. Setup colors and scales
n <- nrow(polyhedra.7.faces)
polyhedron.colors <- rainbow(n)
polyhedron.scale <- 5

# 3. open and setup RGL window
open3d()
par3d(FOV = 1)
rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
rgl.viewpoint(theta = 0, phi=0, zoom=0.8, fov=1)

# 4. for each polyhedron, setup rotation, position and render
for (i in seq_len(n)) {
  # Obtain polyhedron
  polyhedron.row <- polyhedra.7.faces[i,]
  polyhedron.name <- polyhedron.row$scraped.name
  polyhedron <- getPolyhedron(source = polyhedron.row$source, polyhedron.name)

  # Setup angles, position into transformationMatrix
  current.angle <- i/n * 2 * pi
  tm <- rotationMatrix(current.angle, 1, 0, 0)
  x.pos <- round(polyhedron.scale * sin(current.angle), 2)
  y.pos <- round(polyhedron.scale * cos(current.angle), 2)
  tm <- tm %*% translationMatrix(x.pos, y.pos, 0)

  # Render
  print(paste("Drawing ", polyhedron.name, " rotated ", round(current.angle, 2),
              " in (1,0,0) axis. Translated to (", x.pos, ",", y.pos, ",0)",
              " with color ", polyhedron.colors[i], sep = ""))
  shape.rgl <- polyhedron$getRGLModel(transformation.matrix = tm)
  shade3d(shape.rgl, color = polyhedron.colors[i])
}




polyhedra.5.faces <- all.polyhedra[all.polyhedra$faces==5,]

# 2. Setup colors and scales
n <- nrow(polyhedra.5.faces)
n
polyhedron.colors <- rainbow(n)
polyhedron.scale <- 5

# 3. open and setup RGL window
open3d()
par3d(FOV = 1)
rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
rgl.viewpoint(theta = 0, phi=0, zoom=0.8, fov=1)

# 4. for each polyhedron, setup rotation, position and render
for (i in seq_len(n)) {
  # Obtain polyhedron
  polyhedron.row <- polyhedra.5.faces[i,]
  polyhedron.name <- polyhedron.row$scraped.name
  polyhedron <- getPolyhedron(source = polyhedron.row$source, polyhedron.name)

  # Setup angles, position into transformationMatrix
  current.angle <- i/n * 2 * pi
  tm <- rotationMatrix(current.angle, 1, 0, 0)
  x.pos <- round(polyhedron.scale * sin(current.angle), 2)
  y.pos <- round(polyhedron.scale * cos(current.angle), 2)
  tm <- tm %*% translationMatrix(x.pos, y.pos, 0)

  # Render
  print(paste("Drawing ", polyhedron.name, " rotated ", round(current.angle, 2),
              " in (1,0,0) axis. Translated to (", x.pos, ",", y.pos, ",0)",
              " with color ", polyhedron.colors[i], sep = ""))
  shape.rgl <- polyhedron$getRGLModel(transformation.matrix = tm)
  shade3d(shape.rgl, color = polyhedron.colors[i])
}





polyhedra.12.faces <- all.polyhedra[all.polyhedra$faces==12,]

# 2. Setup colors and scales
n <- nrow(polyhedra.12.faces)
n
polyhedron.colors <- rainbow(n)
polyhedron.scale <- 20

# 3. open and setup RGL window
open3d()
par3d(FOV = 1)
rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
rgl.viewpoint(theta = 0, phi=0, zoom=0.8, fov=1)

# 4. for each polyhedron, setup rotation, position and render
for (i in seq_len(n)) {
  # Obtain polyhedron
  polyhedron.row <- polyhedra.12.faces[i,]
  polyhedron.name <- polyhedron.row$scraped.name
  polyhedron <- getPolyhedron(source = polyhedron.row$source, polyhedron.name)

  # Setup angles, position into transformationMatrix
  current.angle <- i/n * 2 * pi
  tm <- rotationMatrix(current.angle, 1, 0, 0)
  x.pos <- round(polyhedron.scale * sin(current.angle), 2)
  y.pos <- round(polyhedron.scale * cos(current.angle), 2)
  tm <- tm %*% translationMatrix(x.pos, y.pos, 0)

  # Render
  print(paste("Drawing ", polyhedron.name, " rotated ", round(current.angle, 2),
              " in (1,0,0) axis. Translated to (", x.pos, ",", y.pos, ",0)",
              " with color ", polyhedron.colors[i], sep = ""))
  shape.rgl <- polyhedron$getRGLModel(transformation.matrix = tm)
  shade3d(shape.rgl, color = polyhedron.colors[i])
}



#dependency
#https://www.playframework.com/documentation/2.6.x/Requirements

install.packages("soundgen")

library("soundgen")
library("tuneR")

soundgen::playme(sound = soundgen::beat())


t <- seq(0, 3, 1/8000) #times in seconds if sample for 3 seconds at 8000Hz
u <- (2^15-1)*sin(2*pi*440*t) #440 Hz sine wave that lasts t length seconds (here, 3 seconds)
w <- Wave(u, samp.rate = 8000, bit=16) #make the wave variable
play(w, play="afplay")

tetrahedron <- getPolyhedron(polyhedron.name = "tetrahedron")

tetrahedron.interpreted <- PolyhedronInterpreted.class$new(polyhedron = tetrahedron)
self <- tetrahedron.interpreted
tetrahedron.interpreted$numerateFaces()
tetrahedron.interpreted$faces.number


all.polyhedra <- getAvailablePolyhedra()
all.polyhedra %>% filter(grepl("truncated",scraped.name))




#select simple polyhedra

all.polyhedra <- getAvailablePolyhedra()


polyhedra.music <- all.polyhedra[all.polyhedra$faces<=8,]
rownames(polyhedra.music) <- 1:nrow(polyhedra.music)
polyhedra.music %>% filter(faces==6)

polyhedra.music[grepl("prism",polyhedra.music$scraped.name),]

set.seed(2)
sampled <- sort(sample(1:nrow(polyhedra.music),size = 9,replace = FALSE))
sampled <- sort(c(sampled, 58))
polyhedra.music.sampled <- polyhedra.music[sampled,]
n <- nrow(polyhedra.music.sampled)
polyhedron.colors <- rainbow(n)
polyhedron.scale <- 5



open3d()
par3d(FOV = 1)
rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
rgl.viewpoint(theta = 0, phi=0, zoom=0.8, fov=1)

# 4. for each polyhedron, setup rotation, position and render
for (i in seq_len(n)) {
  # Obtain polyhedron
  polyhedron.row <- polyhedra.music.sampled[i,]
  polyhedron.name <- polyhedron.row$scraped.name
  polyhedron <- getPolyhedron(source = polyhedron.row$source, polyhedron.name)

  # Setup angles, position into transformationMatrix
  current.angle <- i/n * 2 * pi
  tm <- rotationMatrix(current.angle, 1, 0, 0)
  x.pos <- round(polyhedron.scale * sin(current.angle), 2)
  y.pos <- round(polyhedron.scale * cos(current.angle), 2)
  tm <- tm %*% translationMatrix(x.pos, y.pos, 0)

  # Render
  print(paste("Drawing ", polyhedron.name, " rotated ", round(current.angle, 2),
              " in (1,0,0) axis. Translated to (", x.pos, ",", y.pos, ",0)",
              " with color ", polyhedron.colors[i], sep = ""))
  shape.rgl <- polyhedron$getRGLModel(transformation.matrix = tm)
  shade3d(shape.rgl, color = polyhedron.colors[i])
}
print(sampled)





#Generate timbre

truncated.octahedron <- getPolyhedron(source = "dmccooey", polyhedron.name = "truncated octahedron")

truncated.octahedron.interpreted <- PolyhedronInterpreted.class$new(polyhedron = truncated.octahedron)
self <- truncated.octahedron.interpreted
truncated.octahedron.interpreted$numerateFaces()
truncated.octahedron.interpreted$faces.number

polyhedron.interpreted

timbre <- PolyhedronTimbre.class$new(tetrahedron.interpreted)
self <- timbre
timbre$generate()

max(timbre$timbre)
min(timbre$timbre)

timbre$getWave()

#for 16-bit Wave files, data range is supposed to be in [-32768, 32767], see ?normalize

play(timbre$getWave(), "afplay")

#harmonics test

440*30



for (i in seq_len(n)) {
#for (i in 1:2) {
  # Obtain polyhedron
  polyhedron.row <- polyhedra.music.sampled[i,]
  polyhedron.name <- polyhedron.row$scraped.name
  polyhedron <- getPolyhedron(source = polyhedron.row$source, polyhedron.name)
  polyhedron.interpreted <- PolyhedronInterpreted.class$new(polyhedron = polyhedron)
  polyhedron.interpreted$numerateFaces()

  timbre <- PolyhedronTimbre.class$new(polyhedron.interpreted)
  timbre$generate()
  self <- timbre

  names(timbre$harmonics)
  timbre$getWave()

  #for 16-bit Wave files, data range is supposed to be in [-32768, 32767], see ?normalize

  play(timbre$getWave(), "afplay")
  #debug
#  stop("debug")
}


#Test with complex polyhedra
all.polyhedra <- getAvailablePolyhedra()


polyhedra.music.high.faces <- all.polyhedra[all.polyhedra$faces==54,]
rownames(polyhedra.music.high.faces) <- 1:nrow(polyhedra.music.high.faces)
n <- nrow(polyhedra.music.high.faces)


for (i in seq_len(n)) {
  #for (i in 1:2) {
  # Obtain polyhedron
  polyhedron.row <- polyhedra.music.high.faces[i,]
  polyhedron.name <- polyhedron.row$scraped.name
  polyhedron <- getPolyhedron(source = polyhedron.row$source, polyhedron.name)
  polyhedron.interpreted <- PolyhedronInterpreted.class$new(polyhedron = polyhedron)
  polyhedron.interpreted$numerateFaces()

  timbre <- PolyhedronTimbre.class$new(polyhedron.interpreted)
  self <- timbre
  timbre$generate()
  print(paste("For polyhedron", polyhedron.row$scraped.name, "harmonic mapping is", paste(timbre$harmonics.mapping,collapse = ",")))
  print(paste("For polyhedron", polyhedron.row$scraped.name, "max.amp is", max(timbre$timbre)))

  timbre$getWave()

  #for 16-bit Wave files, data range is supposed to be in [-32768, 32767], see ?normalize

  play(timbre$getWave(), "afplay")
  #debug
  #  stop("debug")
}




poly.chance <- PolyhedronChance.class$new(polyhedron)
self <- poly.chance
poly.chance$initChances()
poly.chance$takeChances(20,1212)


#Now plays timbre with chance sequence
for (i in seq_len(n)) {
  #for (i in 1:2) {
  # Obtain polyhedron
  polyhedron.row <- polyhedra.music.sampled[i,]
  polyhedron.name <- polyhedron.row$scraped.name
  polyhedron <- getPolyhedron(source = polyhedron.row$source, polyhedron.name)
  polyhedron.interpreted <- PolyhedronInterpreted.class$new(polyhedron = polyhedron)
  polyhedron.interpreted$numerateFaces()

  timbre <- PolyhedronTimbre.class$new(polyhedron.interpreted)
  timbre$generate()
  self <- timbre
  print(paste("For polyhedron", polyhedron.row$scraped.name, "harmonic mapping is", paste(timbre$harmonics.mapping,collapse = ",")))
  print(paste("For polyhedron", polyhedron.row$scraped.name, "max.amp is", max(timbre$timbre)))

  timbre$getWave()

  #for 16-bit Wave files, data range is supposed to be in [-32768, 32767], see ?normalize

  poly.chance <- PolyhedronChance.class$new(polyhedron)
  self <- poly.chance
  poly.chance$initChances()
  notes.sequence <- poly.chance$takeChances(20,seed = 1212)

  #debug
  stop("debug")
  self <- timbre
  timbre$playSequence(notes.sequence = notes.sequence, duration =0.5)
  #play(timbre$getWave(), "afplay")
  #debug
  #  stop("debug")
}



