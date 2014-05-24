# the functions below will return the inverse of a matrix.
# they will calculate the value initially and return from cache in subsequent calls
# please note the complete instructions for testing at the end of the code

# makeCacheMatrix returns a list of functions: set (set the value of the matrix), 
#                                              get (get the value of the matrix), 
#                                              setinverse (set the value of the inverse), 
#                                              getinverse (get the value of the inverse)
#
# to test, assign to an object a square matrix and pass that object to the function, as shown below
#
# myMatrix <- matrix(1:4,2,2)
# runFirst <- makeCacheMatrix(myMatrix)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {             # note: <<- will assign a value to an object in an environment that is 
                                     #           different from the current environment
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

# cacheSolve returns the inverse of a matrix.  If the inverse is already cached, it uses that instead of recalculating
#
# cacheSolve(runFirst)
# #### this will return the inversed matrix
# cacheSolve(runFirst)
# #### this will return the inversed matrix and a message saying it retrieved it from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                # call the getinverse function from the makeCacheMatrix returned list
  if(!is.null(m)) {                  # if the value is not null, return from cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()                    # call the get function from the makeCacheMatrix returned list
  m <- solve(data, ...)
  x$setinverse(m)                    # call the setinverse function from the makeCacheMatrix returned list
  m
}

# for ease of testing, the instructions found above are included in a more succinct list below 
# (remember to remove the single leading hash tags)
#
# #### to test, create a matrix and pass it to the makeCacheMatrix function as follows
# myMatrix <- matrix(1:4,2,2)
# runFirst <- makeCacheMatrix(myMatrix)
# cacheSolve(runFirst)
# #### this will return the inversed matrix
# cacheSolve(runFirst)
# #### this will return the inversed matrix and a message saying it retrieved it from the cache
