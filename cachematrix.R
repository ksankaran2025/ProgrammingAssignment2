## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function creates a special "matrix" object that can cache
## its inverse

load(MASS)

makeCacheMatrix <- function(x = matrix()) {

        # set inverse to NULL
        inv <- NULL

        #set function sets the value of x original matrix and
        # resets the  inv
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        # returns x the original matrix
        get <- function() x

        # cache the inverse
        setinv <- function(ainv) {
               message("Setting cached data inverse")
               inv <<-ainv
        }

        # return cached inverse
        getinv <- function() inv

        #list of all functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then the
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        #check if cache has it
        inv <- x$getinv()

        if(!is.null(inv)) {
                #got it in cache
                message("getting cached data inverse")
                return(inv)
        }

        #get the matrix
        data <- x$get()

        #and caclulate invere
        inv <- solve(data)

        #set inverse in cache
        x$setinv(inv)
        inv
}
