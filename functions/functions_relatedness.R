
############################################################################
################### NEW FUNCTION
############################################################################

## Helper function
create_sparse_matrix <- function(i_input, j_input, weight = NULL, projection = NULL, sparse = TRUE){
  require(Matrix)
  
  i_input <- factor(i_input)
  j_input <- factor(j_input)
  
  if (is.null(weight)) {
    value <- rep(1, length(i_input) )
  } else {
    value <- weight
  }
  
  # Fill matrix
  mat <- spMatrix(nrow = n_distinct(i_input),
                  ncol = n_distinct(j_input),
                  i =  as.numeric(i_input),
                  j =  as.numeric(j_input),
                  x = value)
  rm(value)
  
  # Colnames
  rownames(mat) <- i_input %>% levels()
  colnames(mat) <- j_input %>% levels()
  
  # PRojection
  if(projection == 'i') { mat <- tcrossprod(mat) }
  if(projection == 'j') { mat <- crossprod(mat) }  
  
  # Sparsity
  if(sparse == FALSE) { mat <- as.matrix(mat) }  
  
  return(mat)
}


############################################################################
################### NEW FUNCTION
############################################################################

## Helper function
create_sparse_matrix <- function(i_input, j_input, weight = NULL, projection = NULL, sparse = TRUE){
  require(Matrix)
  
  i_input <- factor(i_input)
  j_input <- factor(j_input)
  
  if (is.null(weight)) {
    value <- rep(1, length(i_input) )
  } else {
    value <- weight
  }
  
  # Fill matrix
  mat <- spMatrix(nrow = n_distinct(i_input),
                  ncol = n_distinct(j_input),
                  i =  as.numeric(i_input),
                  j =  as.numeric(j_input),
                  x = value)
  rm(value)
  
  # Colnames
  rownames(mat) <- i_input %>% levels()
  colnames(mat) <- j_input %>% levels()
  
  # PRojection
  if(projection == 'i') { mat <- tcrossprod(mat) }
  if(projection == 'j') { mat <- crossprod(mat) }  
  
  # Sparsity
  if(sparse == FALSE) { mat <- as.matrix(mat) }  
  
  return(mat)
}

