GetBestHospitalinState <- function (HospitalState, MedicalOutcome, HospitalRank=1) 
{
        if(missing(HospitalState))                                              # Exit if State not specified 
                { message("ERROR: State not specifed.") 
                return() 
                } 
        if(missing(MedicalOutcome))                                             # Exit if Medical Condition not specified
                { message("ERROR: Medical Outcome not specifed.") 
                return() 
                }
        setwd("~/GitHub/ProgrammingAssignment3")                                # Get hospital data from subdir...
        HospitalInput <- read.csv("outcome-of-care-measures.csv", 
                                  na.strings="Not Available",                   # Convert na's
                                  colClasses = "character")
        Hospital.df<-data.frame(cbind(HospitalInput[,1],HospitalInput[,2]),     # Just get the hospital factors... 
                                HospitalInput[,7],
                                as.numeric(HospitalInput[,11]),                 # ...and mortality numerics
                                as.numeric(HospitalInput[,17]),
                                as.numeric(HospitalInput[,23]))
        colnames<-c("HospitalCode","Hospital","State","Heart Attack",
                    "Heart Failure","Pneumonia")                                # Add column names
        colnames(Hospital.df)<-colnames
        Hospital.df<-Hospital.df[order(Hospital.df$State,Hospital.df$Hospital),]# Sort the DF by State, Hospital Name
        Hospital.df<-Hospital.df[Hospital.df$State==HospitalState,]             # Limit to user-specified state
        HospitalsInState<-nrow(Hospital.df)                                     # Tally of hospitals in user-selected state
        
        AllStates<-as.vector(unique(Hospital.df$State))                         # Get a list of valid states
        if (length(grep(toupper(HospitalState), AllStates))<1)   # Is user-specified HospitalState valid?
                { message("ERROR: Not a valid U.S. State.")                     # *** not handling lower case very well! ***
                return()
                }     
        
        if (toupper(MedicalOutcome) == "PNEUMONIA") 
                { Output<-head(Hospital.df<-Hospital.df
                          [order(Hospital.df$State,Hospital.df$Pneumonia),])
                } else
        if (toupper(MedicalOutcome) == "HEART ATTACK") 
                { Output<-head(Hospital.df<-Hospital.df
                [order(Hospital.df$State, Hospital.df$'Heart Attack'),]) 
                } else
        if (toupper(MedicalOutcome) == "HEART FAILURE") 
                { Output<-head(Hospital.df<-Hospital.df
                [order(Hospital.df$State, Hospital.df$'Heart Failure'),]) 
                } else
                { message(paste("ERROR: Unrecognized Medical Condition. Please", 
                          "specify 'Heart Attack' or 'Heart Failure' or",
                          "'Pneumonia'"))
                  return()
                }

        return(Output[HospitalRank,])  #Return the data for the hospital of user-specified rank (Best=default=1)
}


