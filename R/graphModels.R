#######
#EXPERIMENTAL CODE FOR GRAPHING MODELS USING GRAPHVIZ






#test <- extractModelParameters("C:\\Program Files\\Mplus\\Mplus Examples\\User's Guide Examples\\ex5.1.out")

addNode <- function(dotModel, name, role, type) {
  if (!inherits(dotModel, "list")) stop("dotModel parameter must be a list")
  
  if (is.null(dotModel[[name]])) dotModel[[name]] <- list(role=role, type=type)
  else {
    #okay to convert something we thought was observed to latent (but not the other way
    if (dotModel[[name]]$type == "observed" && type == "latent") dotModel[[name]]$type <- type			
    
    #append the role if it's not already present (vars can have many roles)
    if (!role %in% dotModel[[name]]$role) dotModel[[name]]$role <- c(dotModel[[name]]$role, role) 
  }
  
  return(dotModel)
}

connectNodes <- function(dotModel, node1, node2, connectionType) {
  
}

graphModel <- function(model) {
  require(plyr)
  if (!inherits(model, "data.frame")) stop("Parameter model must be a data.frame")
  
  byOnWith <- grep("\\.(BY|ON|WITH)$", model$paramHeader, perl=TRUE)
  
  #create a df with name1, connectiontype, name2
  dotModel <- list(nodes=list(), connections=list())
  connections <- a_ply(model, 1, function(row) {
        splitHeader <- strsplit(row$paramHeader, ".", fixed=TRUE)[[1]]
        varName1 <- paste(splitHeader[-length(splitHeader)], collapse=".")
        connectType <- splitHeader[length(splitHeader)]
        varName2 <- row$param
        
        if (connectType == "ON") {
          dotModel$nodes <<- addNode(dotModel$nodes, name=varName1, role="outcome", type="observed")
          dotModel$nodes <<- addNode(dotModel$nodes, name=varName2, role="predictor", type="observed")
          dotModel$connections <<- connectNodes(dotModel$connections, varName1, varName2, "<-")
          
        }
        else if (connectType == "WITH") {
          dotModel <<- addNode(dotModel$nodes, name=varName1, role="covariance", type="observed")
          dotModel <<- addNode(dotModel$nodes, name=varName2, role="covariance", type="observed")
          dotModel$connections <<- connectNodes(dotModel$connections, varName1, varName2, "<->")
        }
        else if (connectType == "BY") {
          dotModel <<- addNode(dotModel$nodes, name=varName1, role="factor", type="latent")
          dotModel <<- addNode(dotModel$nodes, name=varName2, role="indicator", type="observed")
          dotModel$connections <<- connectNodes(dotModel$connections, varName1, varName2, "->")
        }
      })
  
#	varTypes <- ldply(strsplit(byOnWith, ".", fixed=TRUE), function(element) {
#				varName <- paste(element[-length(element)], collapse=".")
#				if (element[length(element)] == "BY") return(data.frame(name=varName, type="latent"))
#				else return(data.frame(name=varName, type="observed"))
#			})
#
#	
#	latentVars <- na.omit(unique(latentVars))
  
#	print(latentVars)
#	dotModel <- addNodes(list(), data.frame(name=latentVars, type="latent", stringsAsFactors=FALSE))
  
  return(dotModel)	
}
