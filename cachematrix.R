# Caching for inverted matrices
# (sorry for my bad english)

# Special function for collecting matrices and their inversions

makeCacheMatrix <- function(inputMatrix = matrix()) {
    invertedMatrix <- NULL
    set <- function(x) {
        inputMatrix <<- x
        invertedMatrix <<- NULL
    }
    get <- function() inputMatrix
    setInverse <- function(solve) invertedMatrix <<- solve
    getInverse <- function() invertedMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Getting inversion of a matrix from cache if is set 
# or calculates inversion caches it and returns it

cacheSolve <- function(inputMatrix, ...) {
    invertedMatrix <- inputMatrix$getInverse()
    if(!is.null(invertedMatrix)) {
        return(invertedMatrix)
    }
    data <- inputMatrix$get()
    invertedMatrix <- solve(data, ...)
    inputMatrix$setInverse(invertedMatrix)
    invertedMatrix
}
