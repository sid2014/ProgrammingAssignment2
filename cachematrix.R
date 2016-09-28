## Below functions will prevent calculating the inverse of a same matrix over and over again if it was passed through arguments
## This will be achieved, checking if cache exists before calculating 
## If not, caching the calculated value everytime once the calculation is done

## The below function will prepare a list of input matrix and its cached inverse

makeCacheMatrix <- function(ip_matrix = matrix()) {
        
 # Check if the input is a matrix or not !      
        
        if(class(ip_matrix) != "matrix") 
        { stop("This is only for matrices") }
        
        dimensions <- dim(ip_matrix)
        
 # Check if solve function can be applied on the input matrix 
        
        if( dimensions[1] != dimensions[2] )
        {
                stop(" General inverse is not possible with this matrix ")
        }
        
        cache_matrix <- NULL
        get_matrix <- function() ip_matrix
        set_matrix_inverse <- function(cache_inverse) cache_matrix <<- cache_inverse
        get_matrix_inverse <- function() cache_matrix

# Prepre input to the cacheSolve function    
                
        list(get_matrix = get_matrix,
             set_matrix_inverse = set_matrix_inverse,
             get_matrix_inverse = get_matrix_inverse)
}

## The below function will reuse the cache value if possible. If not, will cache the calculated value for next use 
                
cacheSolve <- function(makeCacheMatrix_op) {
        
        cache_matrix <- makeCacheMatrix_op$get_matrix_inverse()

# Check if cache exists or not        
        
        if(!is.null(cache_matrix)) 
        {
                message("getting cached data")
                return(cache_matrix)
        }
# If no cache data found, then pick the matrix from the input list and apply sove function
        data <- makeCacheMatrix_op$get_matrix()
        cache_matrix <- solve(data)
        
# Cache the calcuated value, so that we can pick it up if the same input is passed again        
        
        makeCacheMatrix_op$set_matrix_inverse(cache_matrix)
        cache_matrix
}
