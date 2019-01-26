## Put comments here that give an overall description of what your
## functions do

## create Matrix

makeCacheMatrix <- function(x = matrix()) {
    
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## inverse Matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$getinverse()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
}
