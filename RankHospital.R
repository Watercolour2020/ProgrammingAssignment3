RankHospital <- function (HospitalState, MedicalOutcome, num="best") 
{
        if(missing(HospitalState))                                              # Exit if State not specified 
                { message("ERROR: State not specifed.") 
                return() 
                } 
        if(missing(MedicalOutcome))                                             # Exit if Medical Condition not specified
                { message("ERROR: Medical Outcome not specifed.") 
                return() 
                } else ColName <- toupper(MedicalOutcome)
        
        setwd("~/GitHub/ProgrammingAssignment3")                                # Get hospital data from subdir...
        HospitalInput <- read.csv("outcome-of-care-measures.csv", 
                                  na.strings="Not Available",                   # Convert na's
                                  colClasses = "character")
        colName <- toupper(MedicalOutcome)

        if (colName == "PNEUMONIA") 
                { ColumnToGet <- 23 
                } else
        if (colName == "HEART ATTACK") 
                { ColumnToGet <- 11 
                } else
        if (colName == "HEART FAILURE") 
                { ColumnToGet <- 17 
        } else
                { message(paste("ERROR: Unrecognized Medical Condition. Please", 
                                "specify 'Heart Attack' or 'Heart Failure' or",
                                "'Pneumonia'"))
                return()
                }        
        
        Hospital.df<-data.frame(cbind(HospitalInput[,2],                        # Just get the hospital factors... 
                                HospitalInput[,7],
                                as.numeric(HospitalInput[,ColumnToGet])))           # ...and mortality numerics
                                
        colnames<-c("Hospital","State", "Data")                                # Add column names
        colnames(Hospital.df) <- colnames
        Hospital.df<-Hospital.df[order(Hospital.df$State,Hospital.df$Hospital),]# Sort the DF by State, Hospital Name
        Hospital.df<-Hospital.df[Hospital.df$State==HospitalState,]             # Limit to user-specified state
        HospitalsWithData <- Hospital.df[complete.cases(Hospital.df),]
        HospitalsWithData <- HospitalsWithData[order(HospitalsWithData$Data),]
        HospitalsInState<-nrow(HospitalsWithData)                     # Tally of hospitals in user-selected state
   
        
             
        if (toupper(num) == "BEST") 
                { HospitalRank <- 1
                } else
        if (toupper(num) == "worst") 
                { HospitalRank <- HospitalsInState
                } else
        if (num >= HospitalsInState )
                { HospitalRank <- HospitalsInState
                } else HospitalRank <- num
                
message(paste("No. hospitals in state is", HospitalsInState, ". Rank requested is", num))

       AllStates<-as.vector(unique(HospitalInput$State))                          # Get a list of valid states
        if (length(grep(toupper(HospitalState), AllStates))<1)                  # Is user-specified HospitalState valid?
                { message("ERROR: Not a valid U.S. State.")                     # *** not handling lower case very well! ***
                return()
                }     
        
 #       if (colName == "PNEUMONIA") 
                 Output<-HospitalsWithData
  #              [order(HospitalsWithData$State,HospitalsWithData$PNEUMONIA),]
   #             } else
  #      if (colName == "HEART ATTACK") 
    #            { Output<-HospitalsWithData
     #           [order(HospitalsWithData$State, HospitalsWithData$'HEART ATTACK'),])
      #          } else
   #     if (colName == "HEART FAILURE") 
       #         { Output<-HospitalsWithData
        #        [order(HospitalsWithData$State, HospitalsWithData$'HEART FAILURE'),] 
         #       } else
          #      { message(paste("ERROR: Unrecognized Medical Condition. Please", 
           #               "specify 'Heart Attack' or 'Heart Failure' or",
            #              "'Pneumonia'"))
             #     return()
              #  }

      
        return(Output[HospitalRank,])  #Return the data for the hospital of user-specified rank (Best=default=1)
}


