## The makeCacheMatrix makes an object which stores the matrix, its inverse. It returns a list
## The cacheSolve function takes a "special matrix" made with the makeCacheMatrix and returns its inverse.
##If the inverse has already been calculated in the "special matrix" it would just call it. If it is NULL it will calculate it
##and store it in the "special matrix" 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # In inverse would be stored the inverse of the given matrix
        set <- function(new_matrix){
                x <<- new_matrix #The matrix in the object will be changed to one in the argument
                inverse <<- NULL #As the matrix changed, the inverse would be different and needs to be cleaned
        }
        get <- function() x #gets the stored matrix when called
        setinverse <- function(giveninverse) inverse <<- giveninverse #stores the inverse calculated in cacheSolve
        getinverse <- function()inverse #gets the stored inverse when called
        
        list(set=set,get=get,setinverse=setinverse, getinverse=getinverse) #returns a list with the functions from the object
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse() #gets the inverse stored in the x "special matrix"
        if(!is.null(inverse)){ #if there is inverse stored it takes it
                message("getting cached data")
                return(inverse)
        }
        #If the inverse is Null it calculates it, stores it in the object and returns it
        data <- x$get()
        inverse <- solve(data,...)
        x$setinverse(inverse)
        inverse
}
