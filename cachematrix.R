## CREATED BE: Evgeny Chudaev
## CREATED DATE: 2024-05-20
## PURPOSE: Assignment 2 of Stats with R course at Johns Jopkins Stats Specialization @ Coursera

## This function creates a special "matrix" object that can cache its inverse.
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


# Test matrix inversion and caching functions
my_matrix <- matrix(c(2,2,7,8),nrow=2,ncol =2)
#class(my_matrix)

my_matrixx <- makeCacheMatrix(my_matrix)
cacheSolve(my_matrixx)

#solve(my_matrix)
