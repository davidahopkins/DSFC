circ_area <- function(radius){
  pi*(radius^2)
}

circ_area(2)

surf_pipe <- function(radius, height){
  circ <- (pi*radius)*2
  circ * height
}

surf_pipe(3, 10)

surf_beam <- function(flange, web, thickness, long) {
  ttl_flange <- (flange * 2) + ((flange - thickness) * 2)
  ttl_web <- web * 2
  edge <- thickness * 4
  ttl_perim <- ttl_flange + ttl_web + edge
  ttl_perim * long
}

surf_beam(8, 4, 0.5, 10)
