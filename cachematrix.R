## Desription of functions:
## makeCacheMatrix: defines 4 functions and takes user input:
##      set: used to change the input matrix after makeCacheMatrix has been run
##      get: returns the matrix 'x' stored in the main function
##      setmatrix: stores the inverted matrix in the main function
##      getmatrix: returns the inverted matrix to the main function
## cachSolve: checks for a cached value in 'm' and inverts the user input matrix if there is none.

## This function creates 4 functions: set, get, setmatrix, getmatrix.
## These are used to save & retreive user inputs and save & retrieve the function results

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  ## this function is only needed if the matrix is to be changed
                x <<- y       ## changes the matrix stored in the main function
                m <<- NULL    ## restores m to NULL because it is no longer needed
        }
        get <- function() x                       ## return the value x to the main function
        setmatrix <- function(solve) m <<- solve  ## store the inverted matrix in the main function
        getmatrix <- function() m                 ## return the inverted matrix to the main function
        list(set = set, get = get,                ## store the functions in a list
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## this function checks for a cached value in m; if there is none it inverts the user input matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {   # if m exists and is not NULL, return the stored inverted matrix m
                message("getting cached data")
                return(m)
        }
        data <- x$get()      # else calculate the inverse matrix on the newly stored input matrix
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
