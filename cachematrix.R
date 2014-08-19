# Computing the inverse of a matrix is a costly opperation that is often repeated.
# To reduce the computational cost of repeating this calculation the functions below 
# cache the inverse of the matrix along with the matrix itself so that it can be 
# accessed directly without the need to be recalculated


# makeCacheMatrix takes a numerical matrix 'x' as its input and creates a special list 
# object in which the matrix and its inverse are set (set and setInv commands) and 
# can be accessed directly (get, getInv commands). The function returns the list
# matrix object.

makeCacheMatrix <- function(x = matrix()) {
    Minv<-NULL               # sets inverse to null in defining envrionment
    set<-function(y) {       # creates new matrix in globalEnv and sets inverse to null
        x<<-y
        Minv<<-NULL
    }
    get<-function() x        # function to get original matrix
    setInv<-function(matInv) Minv<<-matInv # function to store calculated inverse in globalEnv
    getInv<-function() Minv  # function to retrieve stored inverse matrix
    list(set=set, get=get, setInv=setInv, getInv=getInv)  # list matrix object returned with four functions as elements
}


# The cacheSolve function takes in a matrix cache list object 'x' created by the 
# makeCacheMatrix function and checks to see if an inverse matrix has already been 
# computed and stored in this object. If yes, it returns this matrix, if not, it 
# computes in the inverse matrix and stores it in the matrix list object. It returns
# inverse matrix of 'x'

cacheSolve <- function(x, ...) {
    matInv<-x$getInv()          # gets stored inverse matrix
    if (!is.null(matInv)) {     # if a stored inverse exists - returns it
        message("getting the cached inverse")
        return(matInv)
    }
    mat<-x$get()                # otherwise, gets original matrix
    matInv<-solve(mat, ...)     # calculates the inverse
    x$setInv(matInv)            # stores in inverse in the matrix list object
    matInv                      # returns the inverse
}