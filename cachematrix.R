## Put comments here that give an overall description of what your functions do

## The function generates a matrix object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){
    inver <- NULL
  
  s <- function(y){
    matrix <<- y
    inver <<- NULL
  }
  
   g <- function(){
   
    matrix
  }
  
    sInver <- function(inverse) {
      inver <<- inverse
  }
  
 
  gInver <- function() {
        inver
  }
  
    list(s = s, g = g,
       sInver = sInver,
       gInver = gInver)
}


## The function figures the inverse of the matrix returned by the "makeCacheMatrix" function.


cacheSolve <- function(x, ...) {
 
  inver <- x$gInver()
  
 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inver)
  }
  
  
  
  data <- x$g()
  
  
  m <- solve(data) %*% data
  
  
  x$sInver(m)
  
  ## returning a matrix that is the inverse of 'x'
  m 
}