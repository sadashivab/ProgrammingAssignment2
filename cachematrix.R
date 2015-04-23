## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## 
## makeCacheMatrix <- function(x = matrix()) {
## 
## }
## Write a short comment describing this function
## 
## cacheSolve <- function(x, ...) {
##         ## Return a matrix that is the inverse of 'x'
## }
## 
#==================================================================
#    
#  ****    Content below is the assignment content  ****
#    
#==================================================================
#  R Programming - Assignment 2
#
#  The following code follows the example provided.
#  To verify the code, use the following commands
#
#  source("cachematrix.R")
#  a <- makeCacheMatrix( matrix(c(1,3,2,4), 2,2) )
#  a$get()
#  cacheSolve(a)
#  cacheSolve(a)
#  
#  The inverse displayed is correct as per the link
#  http://www.mathwords.com/i/inverse_of_a_matrix.htm  
#
#==================================================================
#

makeCacheMatrix <- function(Matrix = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                Matrix <<- y
                invMatrix <<- NULL
        }
        get <- function() Matrix
        setInv <- function(Inv) invMatrix <<- Inv
        getInv <- function() invMatrix
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

cacheSolve <- function(Matrix, ...) {
        invMatrix <- Matrix$getInv()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- Matrix$get()
        invMatrix <- solve(data, ...)
        Matrix$setInv(invMatrix)
        invMatrix
}
