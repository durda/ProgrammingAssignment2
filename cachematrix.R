## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special matrix that is a list containing four functions:
##    1. set: to set the value of the matrix X (if old matrix is overwrited, the inverse is reset so that it is computed for the new matrix the first time the 'getinverse() function is called)
##    2. get: to get the matrix X
##    3. setinverse: to set the value of the inverse (XI) of matrix X
##    4. getinverse: to get the value of the inverse (XI) of matrix X

## cacheSolve receives an object created using makeCacheMatrix function. It gets the value of the inverse
## matrix (XI) stored in the environment and:
##    1. If is not null (XI was previosly computed), then it returns the cache value of XI
##    2. If is null, then it computes the inverse of matrix X and store it in the enviroment so that is cached on following calls

## To test ir run:
## mymatrix = matrix(data = c(2,4,0,3,3,1,4,2,0), nrow = 3, ncol = 3)
## cacheSolve(X)
##            [,1]       [,2] [,3]
## [1,] -0.1666667  0.3333333 -0.5
## [2,]  0.0000000  0.0000000  1.0
## [3,]  0.3333333 -0.1666667 -0.5
## cacheSolve(X)
## getting cached data
##            [,1]       [,2] [,3]
## [1,] -0.1666667  0.3333333 -0.5
## [2,]  0.0000000  0.0000000  1.0
## [3,]  0.3333333 -0.1666667 -0.5



## Write a short comment describing this function
## It creates a special object matrix to cache X and the inverse of X, called XI
makeCacheMatrix <- function(x = matrix()) {
      XI <- NULL
      set <- function(Y) {
            x <<- Y
            XI <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) XI <<- inverse
      getinverse <- function() XI
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
## It returns the inverse of matrix X, returning the cached value if it was previosly computed avoiding
## extra computing
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      inverse
}
