## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#### A cache function is created to which the matrix is handed. As the state of the function is stored, by calling X$get(), 
#### other functions can retrieve the original data, although its not explicitely stored.
#### Then four functions are specified, which are later on used to set the data, retrieve the data, set the inverse and get the inverse from the current state of makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {             
        inv<-NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#### cacheSolve is called on the list created with makeCacheMatrix.
#### Then it checks whether an inverse has already been written into this list. If not the case, the inverse is calculated, written into the cache and then printed.

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
        ## Return a matrix that is the inverse of 'x'
}
