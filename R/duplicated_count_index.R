#' Calculate count or Index of duplicated Values
#' @description This function calculates either the count or index of duplicated values in a data frame.
#' @param x A data frame or a vector that can be coerced into a data frame.
#' @param type A character string specifying the type of calculation to perform. Possible values are "count" (default) or "index".
#' @return If type = "count", it returns a vector with the count of duplicates for each row in the data frame x, excluding the row itself. If type = "index", it returns a vector with the index of duplicates for each row in the data frame x, where the index represents the number of previous duplicates (starting from 1) for each unique combination of values.
#' @export
#' 
#' @examples 
#' # Example 1: Count of duplicated rows
#' #data <- data.frame(a = c(1, 2, 3, 2, 1, 3, 4))
#' #duplicated_count_index(data, type = "count")
#'
#' # Output:
#' # [1] 1 1 1 1 1 1 0
#'
#' # Example 2: Index of duplicated rows
#' #data <- data.frame(a = c(1, 2, 3, 2, 1, 3, 4))
#' #duplicated_count_index(data, type = "index")
#'
#' # Output:
#' # [1] 0 0 0 1 2 1 0
duplicated_count_index<-function(x, type = "count"){
  if(type == "count"){
    #make sure x is a dataframe or can be coerced into a dataframe
    x<-data.frame(x)
    
    #calculate the frequency of each combo. (using plyr:: because the function name is used in other packages so need explicit-ness)
    counts <- plyr::count(x)
    
    #merge onto dataset. Adding a call to suppressMessages() because join() likes to tell you stuff a bit unnecessarily otherwise.
    x <- suppressMessages(plyr::join(x, counts))
    
    #return column. Minus 1 so that the number represents number of other matches (i.e. doesn't include itself); so zero for unique, 1 for 1 match etc
    return(x$freq-1)
  }
  if(type == "index"){
    #make sure x is a dataframe or can be coerced into a dataframe
    x<-data.frame(x)
    
    x$hash <- apply(x, 1, paste, collapse = ";;")
    x$id <- 1:nrow(x)
    
    x <- x[order(x$hash),]
    x$count <- 1
    for(i in 2:nrow(x)){
      x$count[i] <- ifelse(x$hash[i] == x$hash[i-1],
                           x$count[i-1]+1,
                           1)
    }
    x <- x[order(x$id),]
    
    return(x$count)
  }
}
