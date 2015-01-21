## returns a list containing the matrix and if it is set, 
## the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function(){ x }  # anonymous function returns x (the matrix)
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Takes a cached matrix and returns its inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    ## message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  x
}