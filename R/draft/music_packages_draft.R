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

setWavPlayer("/Applications/'QuickTime Player.app'/Contents/MacOS/'QuickTime Player'")

t <- seq(0, 3, 1/8000) #times in seconds if sample for 3 seconds at 8000Hz
u <- (2^15-1)*sin(2*pi*440*t) #440 Hz sine wave that lasts t length seconds (here, 3 seconds)
w <- Wave(u, samp.rate = 8000, bit=16) #make the wave variable
play(w, play="afplay")

