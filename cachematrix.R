## functions to calculate and store the inverse of a matrix to save on processing time

# function that stores a matrix as x and its inverse as m
# can then get and set the matrix and and inverse using 4 methods
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(new_inverse) m <<- new_inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##uses makeCacheMatrix to store a matrix and its inverse in the makeCacheMatrix object passed
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
