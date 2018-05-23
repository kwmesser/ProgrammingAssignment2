# Creates an object containing four methods:
#     set: Assign a matrix A to the object
#     get: Return the matrix stored in the object
#     setInv: Assign the inverse of A in the object
#     getInv: Return the inverse stored in the object
# Will store a nonsquare or noninvertible matrix
makeCacheMatrix <- function(A = matrix())
{
    # inv is initialized to NULL
    inv <- NULL
    
    set <- function(B)
    {
        # When new matrix is stored, inv is set to NULL
        A <<- B
        inv <<- NULL
    }
    
    get <- function()
    {
        A
    }
    
    setInv <- function(inverse)
    {
        # This method will blindly assign any input to inv
        # Only use cacheSolve to assign inverse
        inv <<- inverse
    }
    
    getInv <- function()
    {
        inv
    }
    
    # Return methods as list
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

# Returns the inverse of the matrix stored in x
# Throws error if matrix is not square or not invertible
cacheSolve <- function(x, ...)
{
    # Retrieve previously cached value of inv
    inv <- x$getInv()
    
    if (!is.null(inv))
    {
        # If inv is not NULL, use cached value
        message("Getting cached data")
        return(inv)
    }
    
    # If inv is NULL, calculate inverse of matrix
    inv <- solve(x$get(), ...)
    
    # Store inv in x
    x$setInv(inv)
    
    # Return inv
    inv
}
