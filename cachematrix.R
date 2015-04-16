# The function makeCacheMatrix creates a list with four functions:
# 1 - set: Sets the values of the matrix
# 2 - get: Returns the values of the matrix
# 3 - setinverse: Sets the inverse of the matrix
# 4 - getinverse: Returns the inverse of the matrix
# This list represents a matrix that can store its values and its
# inverse.
# The function has one argument with a default value set to an
# empty matrix, which represents the values of the matrix

# NOTE: the getinverse function doesn't calculate the inverse. It
# just returns the value stored in the function. That value could
# be NULL (initial value) or a matrix.

makeCacheMatrix <- function(x = matrix()) {
    # initializes the inverse to NULL
    inv <- NULL
    
    # sets the matrix and initializes the inverse
    set <- function(y){
        x <<- y
        inv <<- NULL
    }

    # returns matrix
    get <- function(){
        x
    }

    # sets the inverse
    setinverse <- function(inverse){
        inv <<- inverse
    }

    #returns the inverse
    getinverse <- function(){
        inv
    }

    # returns the list with the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The function cacheSolve returns the inverse of a matrix.
# Before computing the inverse, it checks whether that value was 
# already computed, and if that's the case, then returns the cached
# value. Otherwise, the inverse is calculated using the solve function
# and the result is returned.
# The function has a variable number of argument. The first is a matrix
# created with the makeCacheMatrix function. The rest of the argumentes
# are passed to the solve function. 

cacheSolve <- function(x, ...) {
    # we need to retrieve the inverse to check if it's not NULL (cached)
    inv <- x$getinverse()
    if (!is.null(inv)){
        # if the inverse is cached, it's returned
        print("The inverse was cached!")
        return(inv)
    }
    # if it's not, the we compute it
    inv <- solve(x$get(), ...)
    # store the value so we don't need to recompute it
    x$setinverse(inv)
    inv
}
