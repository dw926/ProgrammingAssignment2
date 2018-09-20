## The makeCacheMatrix function creates four functions that can set the value of a matrix,
## pull the value of the current matrix, and set and get the value of a solved matrix as well.
## cacheSolve then utilizes the functions created in makeCacheMatrix to check whether a solved
## matrix is already being stored.  If not, it solves (calculates the inverse of) the currently
## stored matrix.

## This function takes a matrix as an input and proceeds to create functions that allow this
## matrix to be retrieved or set to a new value.  It also creates groundwork for the "solved"
## matrix, i.e. the inverse of the inputted matrix, to be set to a new value and retrieved.
## The list at the end of the function creates names for each function that allow the cacheSolve
## function to more easily call the functions by name.

makeCacheMatrix <- function(matrix = matrix()) {
     mat <- NULL
     set_matrix <- function (y){ 
          matrix <<- y
          mat <<- NULL
     }
     get_matrix <- function () matrix
     set_solved_matrix <- function(solv) mat <<- solv
     get_solved_matrix <- function() mat
     list(set_matrix = set_matrix,
          get_matrix = get_matrix,
          set_solved_matrix = set_solved_matrix,
          get_solved_matrix = get_solved_matrix)
}


## This function takes an object that was created by the makeCacheMatrix function.  It utilizes
## the functions within makeCacheMatrix to check whether a solved matrix has already been cached.
## If so, this function retrieves that solved matrix and returns it without performing any
## calculations.  If a solved matrix has not been cached, this function proceeds to solve it 
## (calculate the inverse of the matrix) and then publishes that calculation as the output.

cacheSolve <- function(x, ...) {
     mat <- x$get_solved_matrix()
     if(!is.null(mat)) {
          message("getting cached data")
          return(mat)
     }
     data <- x$get_matrix()
     inverse <- solve(data, ...)
     x$set_solved_matrix(inverse)
     inverse
}
