## The following pair of functions cache the inverse of a matrix,
## which is usually a costly computation.

## The first function, "makeCacheMatrix" creates a special "matrix" object,
## that is a list containing a function that:
## 1. "set": sets the value of the matrix whose inverse is to be computed.
##    When a matrix is passed as its argument, it is assigned to an object x,
##    and NULL is assigned to an object i.
## 2. "get": gets the value of the matrix
##    It simply returns x, the matrix that has been assigned to x
##    by the function "set". 
## 3. "setinverse": sets the value of the inverse of the matrix after it is computed for the first time.
##    It assigns the value passed as its argument to the object i.
## 4. "getinverse": get the value of the inverse from the previous computation
##    It simply returns the object i.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function, "cacheSolve" calculates the inverse of the special "matrix"
## created by "makeCacheMatrix", unless the inverse has already been computed.
## First the function assigns "getinverse" function of the "Matrix" object
## made by "makeCacheMatrix" to the object i.
## Then the function checks if i has a value assigned to it
## (i.e. whether the inverse has been computed before)
## and if it has, then the inverse that has been computed before is returned
## together with the message that notes that the inverse is from cached data.
## If the object i does not have a value (i.e. the inverse has not been computed before),
## then the inverse is calculated by the standard R function "solve",
## and the inverse is set in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}