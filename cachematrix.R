# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix

makeCacheMatrix<- function(x = matrix()) {
    #Initialized the cache to NULL    
        cache <- NULL
    #Stroe a matrix
        setMatrix <- function(myvalue) {
                x <<- myvalue
                cache<<- NULL
        }
    #Return the stroed matrix
        getMatrix <- function() {x}
    #Cache the give argument          
        cacheInverse <- function(solve) {cache <<- solve}
    #Get the cache value
        getInverse <- function() {cache }
    #Return a list. This list contains all of the functions       
        list(setMatrix = setMatrix,  getMatrix =  getMatrix,
             cacheInverse = cacheInverse,
             getInverse = getInverse)
}





# The cacheSolve function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix

cacheSolve <- function(y, ...) {
#Get the cached value
        Inverse<- y$getInverse()
#If a cached value exists, return it
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
#Otherwise get the matrix, calculate the inverse and store it in cache
        data <- y$getMatrix()
        Inverse <- solve(data, ...)
        y$cacheInverse(Inverse)         
#Return the inverse
        Inverse
}
