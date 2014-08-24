
## The first function, makeCacheMatrix creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        setmatrix <- function(solve) inverse <<- solve
        getmatrix <- function() inverse
        
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## The second function, cacheSolve calcualtes the inverse of the matrix makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getmatrix()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setmatrix(inverse)
        inverse
}
