## makeCacheMatrix attempts to cache the inverse of a matrix.
## makeCacheMatrix checks if the input matrix is square and if not, it stops the execution
## cacheSolve uses the solve function to inverse a matrix, unless the inverse of the given matrix was already cached


## Checks for a squre matrix and if true , applies solve to inverse the matrix and stores the result in a global variable (for caching)

makeCacheMatrix <- function(x = matrix()) {
  ##get the dims of the matrix
  d <- dim(x)
  if(length(d) != 2 ){
    stop("the matrix is not square")
    
  }
  ## check if square matrix
  if(d[1] != d[2] ) {
    stop("the matrix is not square")
     
  }
        
  im <- NULL
  set <- function(y){
    x <<-y
    im <<- NULL
  }
  get <- function() as.matrix(x)
  setInv <- function(solve) im<<- solve
  getInv <- function() im
  list(set=set, get=get, setInv=setInv, getInv=getInv)

}


## Checks to see if the inverse of the matrix given as parameter was "cached". If so, get the value from cache, 
## if not, apply solve to inverse the matrix          

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting data from cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setInv(inv)
  inv
}
