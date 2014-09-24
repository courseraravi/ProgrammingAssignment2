makeCacheMatrix <- function( m = matrix() ) 
{
    i <- NULL
    set <- function( matrix ) 
    {
            m <<- matrix
            i <<- NULL
    }
    get <- function() {	m }

    setInverse <- function(inverse) { i <<- inverse }
    getInverse <- function() { i }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) 
{
    matrixm <- x$getInverse()
    if( !is.null(matrixm) ) 
    {
            message("getting cached data")
            return(matrixm)
    }
    
    data <- x$get()
    matrixm <- solve(data) %*% data
    x$setInverse(matrixm)
    matrixm
}
