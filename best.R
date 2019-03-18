best <- function(state, outcome) {
    ## Read outcome data
    estado <- as.character(state)
    desenlace <- as.character(outcome)
    tabla_desenlaces <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
    ## Check that state and outcome are valid
    if(!(estado %in% tabla_desenlaces[["State"]])){
        stop("invalid state")
    }
    desenlacesValidos <- list("heart attack", "heart failure", "pneumonia")
    if(!(desenlace %in% desenlacesValidos)){
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death
    ordenador <- if(desenlace == "heart attack"){
        11
    } else if(desenlace == "heart failure"){
        17
    } else {
        23
    }
    desenlacesEstado <- subset(tabla_desenlaces, 
                               tabla_desenlaces[["State"]] == estado)
    desenlacesOrdenados <- desenlacesEstado[order(
        desenlacesEstado[,ordenador]),]
    ## rate
    return(desenlacesOrdenados[1,2])
    ##return(head(desenlacesOrdenados))
}