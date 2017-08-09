π = pi

degrees <- function(x){
  return((x*180)/π)
}

radians <- function(x){
  return((x*π)/(180))
}

euclidean.dist <- function(v1,v2){
  return(sqrt(sum((v1-v2)^2)))
}

dot.product <- function(v1,v2){
  return(v1[1]*v2[1]+v1[2]*v2[2])
}

magnitude <- function(v){
  return(sqrt(v[1]^2+v[2]^2))
}

heading <- function(v){
  return(-1*atan2(-v[2],v[1]))
}

angle.between <- function(v1,v2){
  return(acos(dot.product(v1,v2)/(magnitude(v1)*magnitude(v2))))
}

mid.point <- function(v1,v2){
  return((v1+v2)/2)
}

map <- function(value,istart,istop,ostart,ostop){
  return(ostart+(ostop-ostart)*((value-istart)/(istop-istart)))
}

normalise <- function(v){
  if(magnitude(v)>0){
    v=v/magnitude(v)
  }
  return(v)
}

rotate <- function(v,c,θ) {
  r.θ = radians(θ)
  cos.r.θ = cos(r.θ)
  sin.r.θ = sin(r.θ)
  x = (cos.r.θ*(v[1]-c[1]))-(sin.r.θ*(v[2]-c[2]))+c[1]
  y = (sin.r.θ*(v[1]-c[1]))+(cos.r.θ*(v[2]-c[2]))+c[2]
  return(c(x,y))
}

scale <- function(v1,v2,scalar){
  v <- v2-v1
  r <- magnitude(v)*scalar
  return (normalise(v)*r)
}
