## makeCacheMatrix creates a matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If inverse is already calculated, cacheSolve will return it.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ##set value of matrix to y
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ##get value of matrix
    get <- function() x
    ##set inverse of matrix to inverse
    setinverse<- function(inverse) i <<-inverse
    ##get value of inverse matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSole will cache the iverse of a given matrix (if it isn't cached yet)
## if it's cached yet, it will return the inverse matrix. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    ##verify if inverse is already cached
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    } else {
        ##if inverse isn't cached, inverse matrix will be calculated, cached and returned
        i <- solve(x$get())
        x$setinverse(i)
        return(i)
    }
}