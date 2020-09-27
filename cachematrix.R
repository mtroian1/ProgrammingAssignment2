## These functions use lexical scoping to create an R object that stores a matrix and its inverse, and then return the inverse that was cached in the parent environment. 
#This helps speed up the retrieval process and prevents repeated calculations.

## makeCacheMatrix uses the function with an argument x=matrix. It sets the value of the matrix, assigns values to the parent environment (clearning previous cache), 
## and then gets the matrix, sets the inverse and saves it, and then assigns a name to each function to use in the next function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL					
	set <- function(y) {				
		x <<- y					
		inv <<- NULL			
	}
	get <- function() x				
	setinverse <- function(inverse) inv <- inverse	
	getinverse <- function () inv			
	list(set =set, get=get,		
	setinverse = setinverse, 
        getinverse = getinverse)
}


## This function retrieves the stored values from makeCacheMatrix. It subsets getinverse from the matrix and if inv is NOT null, it will print a message. 
## if inv IS null, it will retrieve the data from the matrix, find the inverse using solve(data), and then subset and print the data.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()				#sets inv to subset getinverse from the matrix
	if(!is.null(inv)) {					#If m is NOT null and has the inverse...
	message(“getting cached data.”)
        return(inv)					#Return the inverse (need to subset getinverse?)
	}
	data <- x$get()					#If m IS null, need to get data using subset get
	inv <- solve(data)				#store the inverse of data in inv
	x$setinverse(inv)				#subset getinverse
	inv	
}
