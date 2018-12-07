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
      print(neighbours)
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
    #state
    initialize = function(polyhedron.interpreted, base.freq = 440,
                          t = seq(0, 3, 1/8000) #times in seconds if sample for 3 seconds at 8000Hz
                          ){
      self$polyhedron.interpreted <- polyhedron.interpreted
      self$base.freq              <- base.freq
      self$t                      <- t
      self
    },
    generate = function(){
      faces.numbers <- self$polyhedron.interpreted$faces.number
      for (harmonic in seq_len(length(faces.numbers))){
        face <- faces.numbers[[face.order]]
        #current.harmonic <- self$base.freq*
          u <- (2^15-1)*sin(2*pi*440*self$t) #440 Hz sine wave that lasts t length seconds (here, 3 seconds)

        u <- (2^15-1)*sin(2*pi*self$440*t) #440 Hz sine wave that lasts t length seconds (here, 3 seconds)

        print(harmonic)
      }
    },
    makeWave = function(){

      self$wave <- Wave(self$u, samp.rate = 8000, bit=16) #make the wave variable

    })
