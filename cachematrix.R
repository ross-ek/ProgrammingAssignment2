## makeCacheMatrix() is a function that contains two variables and returns a list of four functions. 
## variable x stores the matrix you have entered from the argument to makeCacheMatrix()
##      x is created from makeCacheMatrix argument or by using $set
## Variable m stores the cached inverted matrix... but only after cacheSolve() is called. 
##      m is initialised on first run of makeCacheMatrix or by calling b$set(). This removes the cached matrix. 
## $set will change your stored matrix (var x), if you wanted to
## $get will return your stored matrix (var x)
## $getMatrix will return your inverted matrix after cacheSolve is run (var m)
## $setMatrix will store your inverted matrix after cacheSolve is run (var m)

## call makeCacheMatrix(a)

makeCacheMatrix <- function(x) {
    m <- NULL 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## cacheSolve(b) will call the $getmatrix function contained in our stored function b
##  in order to set a value for m. This value will be NULL if it is the first call
## If x$getmatrix has a value (i.e !is.null()), then the cacheSolve() function will 
## return this and the function ends
## However, if !is.null(m) = FALSE then the following occurs
##      var data will be defined using b$get().. This obtains our original matrix a, stored in var b
##      var m is created using R's solve() function on var data. This is our inverted matrix
##      var m is then "sent back" to our stored function using $setmatrix. 
##      Our stored function, b, now has an updated var m containing our inverted matrix. 

## This is possible due to R's lexical scoping rules that allows a function in R to 
## access other environments outside of itself

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmatrix(m)
    m
}

## Here is a working example:
# > source("cachematrix.R")
# > a <- matrix(1:4,2,2)
# > b <- makeCacheMatrix(a)
# > b$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(b)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(b)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
