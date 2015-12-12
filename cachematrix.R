# [INFO]
# ------
# Coursera - Data Science Specialization
# Course: R Programming
# Programming Assignment 2: Lexical Scoping
# Script author: Aaron Cole
# Script date: 12-Dec-2015


# [PREAMBLE]
# ----------
# The following functions work together to store matrix data and the result of
# inversion of the matrix. They can be used to store and retrieve a cached 
# matrix inversion result.


# [USE]
# -----
# 1. Set a variable for the "makeCacheMatrix()" function.
#       e.g. "m <- makeCacheMatrix()"
#
# 2. Use the "$matrix.set()" function of the variable, passing an invertible 
#    matrix object. If the matrix you provide is not invertible you will get
#    an error later on when inversion is attempted. The "$matrix.set()" function
#    will store the matrix as part of the function's parent object.
#       e.g. "m$matrix.set(matrix(c(4, 3, 3, 2), 2, 2))"
#
# 3. Use the "$matrix.get()" function of the variable to retrieve the stored 
#    matrix.
#       e.g. "m$matrix.get()"
#
# 4. Use the "cacheSolve()" function to retrieve the result of inverting the
#    stored matrix, passing the variable that was used in steps 1 to 3.
#       e.g. "cacheSolve(m)"
#
# 5. If the result of the matrix inversion is cached, then a messge will be 
#    printed as such and the result will be quickly returned from the cache. 
#    Otherwise, the inversion result will need to be calculated and will then 
#    be cached for next time and a message will be printed advising of the 
#    calculation.


# [TESTING]
# ---------
# 1. m <- makeCacheMatrix()
#
# 2. m$matrix.set(matrix(c(4, 3, 3, 2), 2, 2))
#
# 3. m$matrix.get()
#            [,1] [,2]
#       [1,]    4    3
#       [2,]    3    2
#
# 4. cacheSolve(m)
#       Calculating inverse for the first time...
#            [,1] [,2]
#       [1,]   -2    3
#       [2,]    3   -4
#
# 5. cacheSolve(m)
#       Getting data from cache...
#            [,1] [,2]
#       [1,]   -2    3
#       [2,]    3   -4


# See [USE] section for details of how this function is used.
# Returns a list of functions to set and get a matrix and set and get the 
# inverse of the stored matrix.
makeCacheMatrix <- function(x = matrix()) {
    
    # Setup an object to store the inverse.
    matrix.inverse <- NULL
    
    # Set the matrix object and reset the inverse.
    matrix.set <- function(matrix.set.input) {
        
        # Check the input is a matrix.
        if(is.matrix(matrix.set.input)) {
            x <<- matrix.set.input
        } else {
            stop("Parameter must be a matrix.")  
        }

        # We could set the inverse object here, but as it is a costly operation 
        # we will wait until the inverse is actually needed.
        matrix.inverse <<- NULL
    }
    
    # Get the matrix object.
    matrix.get <- function() {
        return(x)
    }
    
    # Set the inverse object.
    matrix.set.inverse <- function() {
        matrix.inverse <<- solve(x)
    }
    
    # Get the inverse object.         
    matrix.get.inverse <- function() {
        return(matrix.inverse)
    }    
    
    # Return a list object of the "special" matrix objects.
    return(list(matrix.set = matrix.set, 
                matrix.get = matrix.get, 
                matrix.set.inverse = matrix.set.inverse,
                matrix.get.inverse = matrix.get.inverse))

}


# See [USE] section for details of how this function is used.
# This function uses a "makeCacheMatrix" object to get a cached result of matrix
# inversion, or trigger the calculation only when necessary and return the 
# inversion result once computed.
cacheSolve <- function(x) {
    
    # Assume x is a makeCacheMatrix object and get its inverse value.
    inverse <- x$matrix.get.inverse()
    
    # If the inverse is not null, then it has already been calculated so we
    # just need to get it.
    if(is.null(inverse) == FALSE) {
        message("Getting data from cache...")
        return(inverse)
    }
    
    # Otherwise at this point we need to do the inverse calculation.
    # If the matrix cannot be solved, then we'll get an error here.
    message("Calculating inverse for the first time...")
    x$matrix.set.inverse()
    
    # If we get this far, we should now be able to get the inverse object.
    return(x$matrix.get.inverse())

}
