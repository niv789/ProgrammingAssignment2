##Matrix inversion is usually a costly computation and 
#there may be some benefit to caching the inverse of a 
##matrix rather than compute it repeatedly
## pair of functions that cache the inverse of a matrix:makeCacheMatrix(),cacheSolve()

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <-function(y){
    x <<-y
    i <<-NULL
  }
  get <-function()x
  setinver <-function(inverse) i<<-inverse
  getinver <-function() i
  list(set=set,get=get,
       setinver=setinver,
       getinver=getinver)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated,retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinver()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <-x$get()
  i <-solve(data, ...)
  x$setinver(i)
  i
}




## -----implementation-----

## >my<-makeCacheMatrix(matrix(1:4,2,2))
## >my$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## >my$getinver()
## NULL

## >cacheSolve(my)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## >cacheSolve(my)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

