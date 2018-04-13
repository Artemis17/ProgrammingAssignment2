#Submission for programming assignment 2.

#Function to create special matrix object
#The umbrella function, makeCacheMatrix() contains functions to:
#	1. construct the matrix
#	2. get the valules of the matrix
#	3. set the inverse of the matrix
#	4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	  matrix_inverse <- NULL
        set <- function(y) { ### Setter function to construct a new matrix
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() x ### Getter function to obtain matrix values
        setinverse <- function(inverse) matrix_inverse <<- inverse ### Setter function for matrix inversion
        getinverse <- function() matrix_inverse ### Getter function for inverse of matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#Function to actually compute inverse of matrix object constructed via makeCacheMatrix(x)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  matrix_inverse <- x$getinverse()
        if(!is.null(matrix_inverse)) {
                message("Getting Cached Data...")
                return(matrix_inverse)
        }
        data <- x$get()
        matrix_inverse <- solve(data, ...)
        x$setinverse(matrix_inverse)
        matrix_inverse
}
