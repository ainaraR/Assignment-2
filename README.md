## This function contains the functions "set", "get", "setinverse", 
##"getinverse" in a list. "<<-" to avoid exposure of variables
##to other enviroments. 

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL ##setting the local variable NULL
		set <- function(y) {
			x <<- y
			m <<- NULL ##initializes m to NULL
		}
		get <- function()x ##return the input matrix
		setinverse <- function(inv) m<<-inv ##sets the inversed matrix
		getinverse <- function()m ##gets the inversed matrix
		list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}
## Computing the inverse matrix

cacheSolve <- function(x, ...) {
		m <- x$getinverse() ##get the inverse matrix
		if(!is.null(m)) { ##if it isn't null R will print the message
			message("getting cached data")
			return(m) ##return the calculated inversion
		}
		data <- x$get()##assign to the data variable a numeric vector
		m <- solve(data)##solve it
		x$setinverse(m)
		m
}

