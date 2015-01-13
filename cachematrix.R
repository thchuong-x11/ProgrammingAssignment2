## Put comments here that give an overall description of what your
## functions do

# Based on the provided example of the special vector, I define
# makeCacheMatrix as a list of functions who 
# - set the value of a matrix
# - get the value of the cached matrix
# - set the value of the matrix's inverse
# - get the value of the inverse

# Then the cacheSolve will either get the matrix inverse if it is
# already cached or calculate the inverse and cache it in the list

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # the cached inverse
        inv <- NULL
        
        # the set and get functions are used to store and get the cached matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        # the setinverse and getinverse are used to store and get the cached invserse
        setinverse <- function(inverse) inv <<- invserse
        getinverse <- function() inv
        list(get = get, set = set, 
             getinverse = getinverse,
             setinverse = setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        # check if the inverse is already calculated,
        # if so, return it without calculation
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # calculate the matrix inverse and store it in
        # the list using setinverse
        mat <- x$getdata()
        inv <- ginv(mat)
        x$setinverse(inv)
        inv
}
