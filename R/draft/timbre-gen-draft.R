nrow(polyhedra.7.faces)
library(rgl)
library(Rpolyhedra)
library(RpolyhedraMusic)

all.polyhedra <- getAvailablePolyhedra()


polyhedra.music.high.faces <- all.polyhedra[all.polyhedra$faces %in% 54:60,]
rownames(polyhedra.music.high.faces) <- 1:nrow(polyhedra.music.high.faces)
n <- nrow(polyhedra.music.high.faces)
n

# Antonio

# Curva ADSR para generar
# trabajar en poner los armonicos superiores en el ataque
#

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
  print(paste("For polyhedron", polyhedron.row$scraped.name, "harmonic mapping is", paste(timbre$harmonics.mapping, collapse = ",")))
  print(paste("For polyhedron", polyhedron.row$scraped.name, "max.amp is", max(timbre$timbre)))

  timbre$getWave()

  #for 16-bit Wave files, data range is supposed to be in [-32768, 32767], see ?normalize

  play(timbre$getWave(), "afplay")
  #debug
  #  stop("debug")
}
