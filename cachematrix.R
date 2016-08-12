
## This function creates a special "matrix" object 
## that can cache its inverse



makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  setinvert <- function(y){
    inv <<- y
  }
  get <- function() x
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  getinvert <- function() inv
  list (get = get, 
        set = set, 
        getinvert = getinvert,
        setinvert = setinvert)
}


## This functin computes the inverse of the special
## "matrix" object returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvert()
  if(!is.na(inv)){
      message("getting cached data")
      return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinvert(inv)
  inv
}
