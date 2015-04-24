# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix. First function
# makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
# Second function cacheSolve computes the inverse of special metrix object
# that can cache its inverse

# The first function makeCacheMatrix creates a special 'vector', which is really
# a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmatrix <<- inverse
  getinverse <- function() invmatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function. It is assumed that matrix is always invertible for the 
# sake of this assignment. 
cacheSolve <- function(x, ...) {
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data)
  x$setinverse(invmatrix)
  invmatrix
}

## Function can be tested by running sample queries below:
# > metr = rbind(c(4, 2), c(2, 4))
# > y = makeCacheMatrix(metr)
# > y$get()
#      [,1] [,2]
# [1,]    4    2
# [2,]    2    4

# During the first run, there is no cached data
# > cacheSolve(y)
#           [,1]       [,2]
# [1,]  0.3333333 -0.1666667
# [2,] -0.1666667  0.3333333

# During the second run, function gets the cached data
# > cacheSolve(y)
# getting cached data
# [,1]       [,2]
# [1,]  0.3333333 -0.1666667
# [2,] -0.1666667  0.3333333
# >