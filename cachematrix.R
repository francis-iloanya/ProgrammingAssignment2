# Programming Assignment 2: Lexical Scoping 

## The first line sets up the function with the x argument being defaulted to a matrix
## Then create an empty object called inverse where the inverse will be stored
## Set function will assign new matrix to X and reset inverse value to NULL ready to be populated
## Get function returns the matrix
## Set inverse when called assigns value of inverse matrix into parent environment
## Get inverse when called retrieves value of inverse matrix


makeCacheMatrix <- function(x=matrix()){       
        inverse <- NULL                         
        set <- function(y) {                 
                x <<- y
                inverse <<- NULL                
        }
        
        get <- function() x                     
        
        
        setinverse <- function(inv) inverse <<- inv  
        getinverse <- function() inverse                
        
        list(set = set, get = get,  
             setinverse = setinverse,
             getinverse = getinverse)
}


## Searches for inverse first - if found, returns value, otherwise continues through rest of the function which...
## ...gets the matrix, solves the inverse, stores it, the prints it

cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("retreiving cached data")
                return(inverse)
        }                                       
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}
