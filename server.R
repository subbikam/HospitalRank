
##rankhospital is a function to compute the best or worst hospital
rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        
        hospital_data <- read.csv(".//HospitalData//hospital-data.csv")
        outcome_data <- read.csv(".//HospitalData//outcome-of-care-measures.csv", colClasses = "character")
        outcome_data[, 11] <- as.numeric(outcome_data[, 11])
        outcome_data[, 17] <- as.numeric(outcome_data[, 17])
        outcome_data[, 23] <- as.numeric(outcome_data[, 23])
        id<-num
        ## Check that state and outcome are valid
        
        if(!state %in% unique(outcome_data[, 7])) 
        {
                stop("Invalid State")
        }
        
        if(outcome !="heart attack" & outcome != "heart failure" & outcome!= "pneumonia")
        {
                stop("Invalid Outcome")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        temp_outcome <- outcome_data[outcome_data$State == state,]
        
        
        if (outcome == "heart attack")
        {
                
                rank<-order(temp_outcome[,11], temp_outcome[,2], na.last = NA)
                
        } 
        
        if (outcome == "heart failure")
        {
                
                rank<-order(temp_outcome[,17], temp_outcome[,2], na.last = NA)
                
        }         
        
        if (outcome == "pneumonia")
        {
                
                rank<-order(temp_outcome[,23], temp_outcome[,2], na.last = NA)
                
        }  
        temp2 <- temp_outcome[rank,]
        
        
        
        if(num=="best")
        { 
                id <- 1
        }
        
        if(num=="worst")
        {
                id <- nrow(temp2)
        } 
        
        
        temp2$Hospital.Name[id]
        
        
}
library(shiny)

shinyServer(
        function(input, output) {
        output$hospital <- renderText({
                input$goButton
                isolate(rankhospital(input$state, input$disease, input$num))
                })
        }
)
