## function cacheSolve: checks whether the inverse of a given matrix was previously computed and cached. 
##If so, it retrieves the inverse and returns it; otherwise, it 
##computes it (by calling solve).
##function makeCacheMatrix: caches the inverse of a given matrix and provides an interface for the manipulation and retrieval of that value.
## Author: Antoniya Petkova

## Caches the inverse of a given matrix and provides an interface for the manipulation and retrieval of that value.
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


## Checks whether the inverse of a given matrix was previously computed and cached. 
##If so, it retrieves the inverse and returns it; otherwise, it 
##computes it (by calling solve on the matrix).
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
