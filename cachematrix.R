# The following functions encapsulate operations performed on matrices 
# and provide a caching mechanism to optimize multiple calculations of
# the inverse of a given matrix

# makeCacheMatrix accepts a matrix objet and creates a list containing 
# functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve accepts a cache matrix object created by makeCacheMatrix
# and returns its inverse. If the inverse was previously calculted,
# the cached value is returned without re-calcutating it. If no cached
# value is found, it is calculated normally and cached for later
# re-use
cacheSolve <- function(x) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
