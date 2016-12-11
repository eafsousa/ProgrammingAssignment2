## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates the special "matrix" object that is in fact a list 
## of functions, including the "get", "set", "setinverse" and "getinverse". 
## The function cacheSolve calculates the matrix inverse and in case it has been previously
## calculated it retrieves it from the cache instead of recalculating it.




## Write a short comment describing this function
## This function creates a list object, where each element corresponds to a function of the
## special object matrix able to cache and retrieve from cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function receives a special matrix object created by function "makeCacheMatrix", 
## computes its inverse and stores it in the cache. If the inverse has been previously 
## calculated and no changes were made to the matrix, then it retrieves the inverse from the 
## cache instead of recalculating it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setinverse(inv)
  inv  

}
