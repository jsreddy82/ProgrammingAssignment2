## makeCacheMatrix creates a special "matrix" object

## The makeCacehMatrix creates a matrix object that contains a list defining 
##    four functions: set, get, setInv and getInv. 
## The set and get funtions set and get values of the matrix 
##    while the setInv and getInv functions set and get the inverse of a given matrix respectively.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The cacheSolve checks if the inverse of the matrix defined above is already calculated. If calculated,
## it returns the inverse, else calculates the inverse using setInv function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}

