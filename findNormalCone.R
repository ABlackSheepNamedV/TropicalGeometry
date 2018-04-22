require(rcdd)

findNormalCone <- function(vectorOfAreaSums, vectorOfInterest) {
  dimension <- dim(vectorOfAreaSums)[2]
  
  polytope.Vrep <- cbind(0,1,vectorOfAreaSums)
  polytope.Hrep <- scdd(polytope.Vrep, representation = "V")
  
  half.space.consts <- polytope.Hrep $ output[,2]
  half.space.vectors <- polytope.Hrep $ output[,-c(1,2)]
  
  active <- half.space.vectors %*% vectorOfInterest + half.space.consts == 0
  
  tangentCone <- polytope.Hrep $ output[active,]
  span.or.cone <- tangentCone[,1] == 1
  
  span.vectors <- tangentCone[span.or.cone, -c(1,2)]
  cone.vectors <- tangentCone[!span.or.cone, -c(1,2)]
  
  projection.matrix <- diag(dimension) - ginv(span.vectors) %*% span.vectors
  cone.vectors.orthogonal <- cone.vectors %*% projection.matrix
  
  return(cone.vectors.orthogonal)
}

VectorOfAreaSums <- rbind( c(1,2,2,1), 
                           c(2,0,3,1), 
                           c(1,3,0,2), 
                           c(3,0,0,3))
vectorOfInterest <- c(1,2,2,1)
findNormalCone(VectorOfAreaSums, vectorOfInterest)
