## The following two functions calculate and cache the inverse of an invertible 
## matrix. 
## The first function (makeCacheMatrix) stores a list of functions (set, get, 
## setinv, getinv), two of which compute and cache the inverse
## The second function uses the list of functions stored by makeCacheMatrix to
## return the inverse of the matrix either from the cache or by computation.


## This function is a list of four functions. getmatr() returns the matrix, 
## setmatr() replaces the matrix with the entered matrix, setinv() computes 
## and caches the inverse and getinv() returns the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        p <- NULL
        setmatr <- function(y) {
            x <<- y
            p <<- NULL
        }
        getmatr <- function() x
        setinv <- function(solve) p <<- solve
        getinv <- function() p
        list(setmatr = setmatr, getmatr = getmatr,
             setinv = setinv,
             getinv = getinv)
}



## The cacheSolve takes the matrix returned by makeCacheMatrix function and 
## first checks if the inverse is already cached and unchanged.  In that case, it
## yields the inverse from the cache.  
## If the cache does not have the inverse, it gets the matrix to be inverted 
## and solves for the inverse and stores this inverse as the cached inverse.

cacheSolve <- function(x, ...) {
      p <- x$getinv()
       if(!is.null(p)) {
             message("getting cached data")
               return(p)
           }
       data <- x$getmatr()
       p <- solve(data, ...)
       x$setinv(p)
       p     

#   Example:
#   b <- matrix(1:4, 2, 2)
#   test1 <- makeCacheMatrix(b)
#   test1$getmatr()
#        [,1] [,2]
#   [1,]    1    3
#   [2,]    2    4
#   test1$getinv()
#   NULL
#   cacheSolve(test1)
#        [,1] [,2]
#   [1,]   -2  1.5
#   [2,]    1 -0.5
#   test1$getinv()
#        [,1] [,2]
#   [1,]   -2  1.5
#   [2,]    1 -0.5
#   cacheSolve(test1)
#   getting cached data
#        [,1] [,2]
#   [1,]   -2  1.5
#   [2,]    1 -0.5
#    test1$getinv()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5     
    
}
