# This function acts as storage environment for the getter/setter functions that 
# would act on the variable passed to the cacheSolve function
 
makeCacheMatrix <- function(x = matrix()) {
  
        # m is the temp variable created in the makeCacheMatrix's environ to store matrix x
        m <- NULL
        
        # sets the value of the 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # gets the value of the matrix in the parent environment- 
        #i.e. the makeCacheMatrix's environment
        get <- function() {x}
        
        # stores the inverse of x in the makeCacheMatrix's environ 
        #as the value passed to the function
        setInv <- function(invMat)
        {
                m <<- invMat
        }
              
        getInv <- function() {m}
        
        # stores and returns the 4 functions defined above
        list(set = set, get = get, setInv = setInv,  getInv = getInv)
}


# This is a wrapper function that would call makeCacheMatrix to solve the mattirx inverse
# after checking if it already exists in the cache. If it does, it returns the cached 
# version else, calculates and returns the calculated inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}


