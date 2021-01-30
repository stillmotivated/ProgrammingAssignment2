## makeCacheMatrix() creates an R object that stores a matrix and its inverse
## casheSolve() requires an argument that is returned by makeCacheMatrix() 
#in order to retrieve the inverse of 'x' from the cached value that is stored 
#in the makeCacheMatrix() object's environment.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        #each time set function is executed, x and inv are reset,
        #if i first create myMatrix with makeCasheMatrix function,
        #I can change the matrix with set function 
        #myMatrix$set(matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2))
        #In this case as matrix has changed the inv value for example will be reset to NULL
        #and will be calculated in casheSolve function anew.
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        #this function is to check the current data = matrix 
        #which is used to calculate the inverse
        get <-  function() {
                x
                }
        #setinv function is for later use in casheSolve to set (=save)
        #the result (=the calculated inverse matrix of x)
        setinv <- function (z) {
                inv <<- z
        }
        #getinv function is for later use in casheSolve to check if the inverse matrix
        #has already been calculated
        getinv <- function () inv
        #creating a named list with functions,
        #to be able to access functions using '$' Symbol
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## Write a short comment describing this function

casheSolve <- function(x, ...) {
        #making use of getinv function, defined in makeCacheMatrix
        #getting the value of inv from makeCacheMatrix and saving it to the local
        #variable inv.local
        inv.local <- x$getinv()
        #if the value is NOT NULL then
        #show the message and return already calculated cashed value
        if(!is.null(inv.local)) {
                message("getting cached data")
                return(inv.local)
        }
        #otherwise get the current matrix and save it in local variable 'data'
        data <- x$get()
        #Calculte the inverse and save it in local variable 'inv.local.calculated'
        inv.local.calculated <- solve(data, ...)
        #set inv in makeCasheMatrix to the inv.local.calculated
        x$setinv(inv.local.calculated)
        inv.local.calculated # Return a matrix that is the inverse of 'x'
}
        
