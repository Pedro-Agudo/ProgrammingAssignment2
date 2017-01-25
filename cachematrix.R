#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#
#   -> set the value of the matrix
#   -> get the value of the matrix
#   -> set the value of the inverse of the matrix
#   -> get the value of the inverse of the matrix

#inv_mat is the variable that stores the inverse of the matrix.
#Thus, we can distinguish between function (solve) and result (matrix)
makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv_mat <<- solve
    getsolve <- function() inv_mat
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


#The following function calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse (inv_mat) has already been calculated. 
#If so, it gets the inverse from the cache ( return(inv_mat) )and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets it in the cache via the setsolve function


cacheSolve <- function(x, ...) {
    inv_mat <- x$getsolve()
    if(!is.null(inv_mat)) {
        message("getting cached data")
        return(inv_mat)
    }
    data <- x$get()
    inv_mat <- solve(data, ...)
    x$setsolve(inv_mat)
    inv_mat
}
