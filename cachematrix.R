## Put comments here that give an overall description of what your
## functions do

## As we know, computing the inverse of matrices is usually computationally expensive
## and thus there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly。

## Write a short comment describing this function
 
## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
# not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  return(m)
}


source("./cachematrix.R")

## test makeCacheMatrix
test_mat <- makeCacheMatrix(matrix(5:8, 2, 2))
test_mat$get()
# [,1] [,2]
# [1,]    5    7
# [2,]    6    8

test_mat$getinv()
# NULL

## Test for cacheSolve: after finding the inverse using casheSolve, 
# then the same inverse of the matrix will be the output of getinv()
cacheSolve(test_mat)
#       [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5

test_mat$getinv()

# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
