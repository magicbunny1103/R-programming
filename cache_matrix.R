#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the invertible matrix
#get the value of the invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set_matrix <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(inverse) inverse_matrix <<- inverse
    get_inverse <- function() inverse_matrix
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


#The following function returns a matrix that is the inverse of x. 
#However, it first checks to see if the invertible matrix has been cached. If so, it gets the matrix from the cache and skips the inverse. Otherwise, it invert the matrix, set and return the invertible matrix.

cacheSolve <- function(x, ...) {
    inverse_matrix <- x$get_inverse()
    if(!is.null(inverse_matrix)) {
        message("getting cached invertible matrix")
        return(inverse_matrix)
    }
    matrix_data <- x$get_matrix()
    inverse_matrix <- solve(matrix_data, ...)
    x$set_inverse(inverse_matrix)
    inverse_matrix
}