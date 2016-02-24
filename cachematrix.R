## Pair of function for creating a special matrix that can cache it's inverse.
## makeCacheMatrix creates a special matrix and the functions necessary to 
## to set and access it's stored inverse.
## cacheSolve calcutates the inverse of a matrix created by makeCacheMatrix
## and stores the inverse in the enviorment of the special matrix.

## Creates special matrix capable of caching it's inverse.

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


## Calculates the inverse of a special cache matrix,
## and stores inverse in matrix cache. Before computing
## inverse, check chache for existing inverse.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinverse(inv) ## Set cache inverse of matrix 'x'
        inv ## Returns a matrix that is the inverse of 'x'
}
