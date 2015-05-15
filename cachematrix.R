
## Set of functions do inverse calculation and caches it 
## Set functions assumes that passed on matrix is always square in nature


# makeCacheMatrix function creates a special "matrix" object that can cache 
# its inverse.
# It creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of matrix
# get the value of the inverse of matrix

makeCacheMatrix <- function(mat = matrix()) {
        #initialize invmat to NULL to ensure that invmat is not calculated yet
        invmat <- NULL 
        #assign matrix
        setmat <- function(matval){
                mat <<- matval
                invmat <<- NULL
        }
        #get matrix
        getmat <- function() {
                mat}
        #set inverse of matrix
        setinvmat <- function(invmatval){
                invmat <<- invmatval
        } 
        #get inverse of matrix
        getinvmat <- function(){
                invmat
        }
        #list containing operations
        list (setmat = setmat, getmat = getmat, 
              setinvmat = setinvmat, getinvmat = getinvmat)

}



# cacheSolve function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
        # cache the variable with inverse from special matrix
        invmat <- mat$getinvmat()
        # if cached value is available then fetch it and return it
        if(!is.null(invmat)){
                message("getting cached inverse of matrix")
                return(invmat)
        }
        # cache value is not available ehnce need to calculate inverse
        # fetch matrix
        matrix1 <- mat$getmat()
        #calculate inverse
        invmat <- solve(matrix1)
        #set inverse of matrix which is calulated
        mat$setinvmat(invmat)
        ## Return a matrix that is the inverse of 'mat'
        invmat
}
