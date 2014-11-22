## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a list of functions
## to be used as an input to the cacheSolve function. makeCacheMatrix
## will first create the list of function that can be called upon 
## to get inputs or get cached inverse calculation already performed.

makeCacheMatrix <- function(x = matrix()) {

# makeCacheMatrix
# fucntion will take invertible matrix and return a list of functions:
# 1. set matrix
# 2. get matrix
# 3. set inverse
# 4. get inverse
# this input will be used for the cacheSolve() function
#  
# x will be the matrix to be inverted
#
  
  m <- NULL
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
   
}


## cacheSolve will take input matrix to take inverse, x
## and check makeCacheMatrix function output has the similar
## inverse calculation already made held in cache. If present
## in cache, cacheSolve will take cached solution instead of
## re-calculating the inverse.

cacheSolve <- function(x, ...) {
        
# Return a matrix that is the inverse of 'x'
  
  
   m <- x$getInv()
   if(!is.null(m)){
     message("getting cached inverse data")
     return(m)
   }
   
   data <- x$get()
   m <- solve(data, ...)
   x$setInv(m)
   m
  
}
