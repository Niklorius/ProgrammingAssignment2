## These two functions takes a matrix, takes the inverse of the matrix, and caches that inverse
## so it can be retrieved quickly.

## makeCacheMatrix takes your desired matrix and gives it 4 functions to allow caching
## or 'storing' of the inverse (which will only be obtained if you run the cacheSolve function).

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #initiates inverse i as NULL, later to be filled in with inverse via cacheSolve
    set <- function(y) { #create function 'set' as a quicker alternative to replace with new matrix
        x <<- yinch      #When replace new matrix, of course inverse would have to be re-NULLED.
        i <<- NULL
    }
    get <- function() x  #create function 'get' to simply retrieve the matrix you initiated
    setinverse <- function(inverse) i <<- inverse #create 'setinverse' function to
                                                  #store a new inverse matrix determined by solveCache
    
    getinverse <- function() i #create 'getinverse' function to retrieve the inverse determined by solveCache
    list(set = set, get = get,     #creates a list of internal functions so that another function
         setinverse = setinverse,  # such as solveCache can call the functions of the matrix you made.
         getinverse = getinverse)
}


## cacheSolve takes the matrix you created via makeCacheMatrix and quickly retrieve the inverse
# stored in a cache that is makeCacheMatrix's i variable. Or if there's nothing in the cache,
# it will formulate the inverse via "solve" function and store it via the matrix's "setinverse" function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ic <- x$getinverse() #Gets the inverse matrix of the matrix (either a matrix, or NULL if you've
                         #never ran cacheSolve on the matrix)
    if(!is.null(ic)) {   #If an inverse matrix has been made, it it return the one from the cache
        message("getting cached data") #via 'getinverse' from previous line.
        return(ic)       #terminates the cacheSolve function if this IF statement is TRUE
                         #(i.e an inverse already exists that is stored in the cache)
    }
    data <- x$get()      #if an inverse matrix for the matrix hasn't been cached, retrieve the matrix  
    ic <- solve(data, ...) #from the retrieved matrix, compute the inverse matrix
    x$setinverse(ic) #store the inverse matrix into the cache via 'setinverse' function
    ic #return the inverse matrix
}

invTest <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
cacheSolve(invTest)
