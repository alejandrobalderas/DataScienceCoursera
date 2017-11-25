
## A Matrix object will be created with the variable xInv to cache the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
          x <<- y
          xInv <<- NULL
     }
    get <- function() x
    setinv <- function(inv) xInv <<- inv
    getinv <- function() xInv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
    
}


## in the first step the existance of the inverse will be checked, if the inverse is already cached the value will be returend
## if not then the inverse will be computed using the solve function and the value then stored with the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xInv <- x$getinv()
    if(!is.null(xInv)) {
          message("getting cached data")
          return(xInv)
    }
    mat <- x$get()
    xInv <- solve(mat)
    x$setinv(xInv)
    xInv
}


# With this code the functions can be easily be tested

A = matrix(c(1,0,4,1,3,4,4,1,0),nrow = 3, byrow = TRUE)
myMat <- makeCacheMatrix(A)
myMat$get() # Should return the value of the matrix
myMat$getinv() # Should return NULL since the value has not been yet computed
cacheSolve(myMat) # computes the inverse of the matrix 

