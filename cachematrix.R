## makeCacheMatrix function:
#' This function uses a matrix variable to assign values to getters and setters. These getters and setters will be
#' used in the next step, the "cacheSolve" function.

makeCacheMatrix <- function(x = matrix()) {     #  Initialization of two objects, x and m. Only x is mandatory.
        m <- NULL
        set <- function(y) {                    # Setter for the matrix.
                x <<- y                         # Assign the input argument to the x object in the parent environment.
                m <<- NULL                      # Assign the value of NULL to the m object in the parent environment.
        }
        get <- function() x                     # Getter for the matrix.
        setinverse <- function(inverse) m <<- inverse # Setter for the inverse of the matrix.
        getinverse <- function() m              # Getter for the inverse of the matrix.
        list(set = set,                         # Assigns each of above functions as an element within a list.
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function:
#' This function calculates the inverse of the matrix created with the "makeCacheMatrix" function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {                       # Checking to see if the mean has already been calculated.
                message("getting cached data")  # If so, it gets the inverse from the cache ...
                return(m)                       # ... and skips the computation, returning the inverse in the cache.
        }
        data <- x$get()
        m <- solve(data, ...)                   # Otherwise, it calculates the inverse of the matrix ...
        x$setinverse(m)                         # ... and sets the value of the inverse in the cache ...
        m                                       # ... and return a matrix that is the inverse of 'x'.
}


## Testing the function
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2) # Creating a matrix
test <- makeCacheMatrix(m1)                             # Putting it in the functions.
test <- cacheSolve(test)
m1 %*% test                                             # Testing the calculation. We should find an inverse matrix.

