makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method the print the matrix
    get <- function() {
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to print the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse
        i
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
    m <- x$getInverse()

    ##Print the inverse if it exists already
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Read the matrix
    data <- x$get()

    m <- solve(data) %*% data
    x$setInverse(m)
    m
}
