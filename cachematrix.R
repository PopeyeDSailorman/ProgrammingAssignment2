## Put comments here that give an overall description of what your
## functions do
#This R Source contains 2 functions: makeCacheMatrix and cacheSolve
#Both functions are described prior to function definitions

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #set inverse to NULL
  #set value of the matrix and its inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x # return original matrix 
  #set the value of the inverse of the original matrix 
  setInv <- function(invers) inv <<- invers 
  getInv <- function() inv # return the inverse matrix
  #create a list of matrix attributes which can be manipulated later
  list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get inverse value of the original matrix
  inv <- x$getInv() 
  # check if the inverse exists
  if(!is.null(inv)) {
    # if inverse exists, return its value
    message("getting cached data")
    return(inv)
  }
  # since inverse is null, get data of original matrix
  matr <- x$get()
  # compute the inverse
  inv <- solve(matr)
  # cache the value of the inverse 
  x$setInv(inv)
  # return the inverse
  inv
}
