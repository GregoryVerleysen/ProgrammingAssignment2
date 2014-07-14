#Assignment 2

##matrix cache & cacheSolve function

### makeCacheMatrix 
#creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  #i for inverse
  i <- NULL
  
  #set matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #get matrix
  get <- function() x
  
  #set inverse
  setinverse <- function(inverse) i <<- inverse
  
  #get inverse
  getinverse <- function() i
  
  #list of matrix functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



### cacheSolve 
#computes the inverse of the matrix. 
#If the inverse is already calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  #if inverse is already calculated
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #if inverse is not yet calculated
  data <- x$get()
  i <- solve(data, ...)
  
  x$setinverse(i)
  
  #inverse
  i
}