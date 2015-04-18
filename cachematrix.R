## makeCacheMatrix and cacheSolve together return either a newly calculated inverse
## of a given matrix or return a cached copy of the inverse of the matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m = NULL

        get = function() x   # function to return the matrix x
        setInvMatrix = function(invMatrix) m <<- invMatrix    # function to set special variable m
        # to previously calculated value
        getInvMatrix = function() m                           # function to return special variable m
        list(get=get,
             setInvMatrix=setInvMatrix,
             getInvMatrix=getInvMatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m = x$getInvMatrix()
        if(!is.null(m)) {                       # if m has already been cached, then return m
                message("getting cached data")
                return(m)
        }
        data1 = x$get()                         # get the matrix stored by makeCacheMatrix
        m = solve(data1, ...)                   # solve same as inv(a) in Matlab; returns inverse of matrix
        x$setInvMatrix(m)                       # essentially calls: makeCacheMatrix$setInvMatrix(m) where m is inverse Matrix
        return(m)

}
