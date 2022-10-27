# edges per tip
adjacency.tree <- function(tree){
  temp <- ape::prop.part(tree)
  result <- matrix(0, nrow = length(tree$tip), ncol = length(temp), dimnames = list(tree$tip.label, tree$node.label))
  for(i in 1:ncol(result)){
    result[temp[[i]],i] <- 1
  }
  return(result)	
}



# smooth function for ggplot 


# poison smooth 
poison_smooth <- function(...) {
  geom_smooth(method = "gam", method.args = list(family = "poisson"), ...)
  # geom_smooth(method = "glm", method.args = list(family = "poisson"), ...) # glm option
}