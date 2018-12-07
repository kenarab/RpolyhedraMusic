library(dplyr)

PolyhedronInterpreted.class <-  R6::R6Class(
  "PolyhedronInterpreted",
  public = list(
    polyhedron = NA,
    #state
    solid = NA,
    #processing faces hierarchy
    base.face = NA,
    faces.number = NA,
    initialize = function(polyhedron){
      self$polyhedron <- polyhedron
      self
    },
    numerateFaces = function(){
      self$polyhedron$state$inferEdges()
      solid <- self$polyhedron$getSolid()
      self$solid <- list()
      for (face.id in 1:length(solid)){
        vertex.in.face <- solid[[face.id]]
        edges.filtered <- lapply(self$polyhedron$state$edges,
                             FUN = function(x){
                               length(intersect(x, vertex.in.face))==2
                             })
        edges.face <- names(edges.filtered[unlist(edges.filtered)])
        self$solid[[as.character(face.id)]] <- edges.face
      }
      #get faces degree
      faces.degree <- lapply(self$solid, FUN = length)
      max.degree <- max(unlist(faces.degree))
      faces.degree <- faces.degree[faces.degree==max.degree]
      self$base.face <- names(faces.degree)[1]

      self$faces.number <- list()
      self$assignNextNumber(self$base.face)
      self$numerateNeighbours(face.id = self$base.face)
    },
    assignNextNumber = function(face.number){
      n <- length(self$faces.number) + 1
      self$faces.number[[n]] <- face.number
      self$faces.number[[n]]
    },
    numerateNeighbours = function(face.id){
      face.edges <- self$solid[[face.id]]
      neighbours.candidates <- vapply(names(self$solid),
                                       FUN = function(x){
                                         length(intersect(self$solid[[x]],
                                                          face.edges)
                                          )>0
                                  }, FUN.VALUE = logical(1))
      #Remove face
      neighbours.candidates[as.numeric(face.id)] <- FALSE
      neighbours.candidates[as.numeric(unlist(self$faces.number))] <- FALSE

      neighbours <- self$solid[neighbours.candidates]
      #debug
      #print(neighbours)
      neighbours.degree <- lapply(neighbours, FUN = length)
      neighbours.df <- data.frame(face.id = names(neighbours.degree),
                                  degree = unlist(neighbours.degree),
                                  stringsAsFactors = FALSE)
      neighbours.df <- neighbours.df %>% arrange(desc(degree), face.id)
      #BFR covering
      for (i in seq_len(nrow(neighbours.df))){
        self$assignNextNumber(neighbours.df[i,"face.id"])
      }
      #Recursive call for each neighbour
      for (i in seq_len(nrow(neighbours.df))){
        self$numerateNeighbours(face.id = neighbours.df[i,"face.id"])
      }

    }
))



PolyhedronTimbre.class <-  R6::R6Class(
  "PolyhedronTimbre",
  public = list(
    polyhedron.interpreted = NA,
    base.freq = NA,
    t = NA,
    #state
    harmonics.mapping = NA,
    harmonics = NA,
    timbre    = NA,
    initialize = function(polyhedron.interpreted, base.freq = 440,
                          t = seq(0, 3, 1/8000) #times in seconds if sample for 3 seconds at 8000Hz,
                          ){
      self$polyhedron.interpreted <- polyhedron.interpreted
      self$base.freq              <- base.freq
      self$t                      <- t
      self
    },
    generate = function(){
      all.harmonics <- 2:30
      poly <- self$polyhedron.interpreted$polyhedron
      solid.faces <- poly$state$solid
      solid.vertices <- poly$state$vertices
      solid.vertices <- solid.vertices[rownames(solid.vertices) %in% unique(unlist(solid.faces)),]
      seed <- length(solid.faces)*151+ nrow(solid.vertices)*137+ length(poly$state$edges)*173
      set.seed(seed)
      faces.numbers <- self$polyhedron.interpreted$faces.number
      harmonics.size <- length(faces.numbers)
      self$harmonics.mapping <- c(1:sample(all.harmonics, size = harmonics.size,replace = FALSE))
      self$harmonics <-  data.frame(t = self$t)
      for (face.order in seq_len(harmonics.size)){
        face <- faces.numbers[[face.order]]
        harmonic <- self$harmonics.mapping[face.order]
        #current.harmonic <- self$base.freq*
        current.harmonic.wave <- (2^15-1)/harmonics.size*sin(2*pi*self$base.freq*harmonic*self$t)
        self$harmonics[,paste("harmonic",harmonic,sep = "_")] <- current.harmonic.wave
        #  u <- (2^15-1)*sin(2*pi*440*self$t) #440 Hz sine wave that lasts t length seconds (here, 3 seconds)

        #u <- (2^15-1)*sin(2*pi*self$440*t) #440 Hz sine wave that lasts t length seconds (here, 3 seconds)
        print(harmonic)
      }
      self$timbre <- apply(self$harmonics[,2:ncol(self$harmonics)],MARGIN = 1, FUN=sum)
      self
    },
    getWave = function(){
      Wave(self$timbre, samp.rate = 8000, bit=16) #make the wave variable
    }))
