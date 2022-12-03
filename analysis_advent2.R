####### PART 1 OF ADVENT CALENDAR EXERCISE #######
rm(list=ls())

# Load data set
df<-read.table("rock_paper_scissors.txt", header=FALSE)

# Change configuration of the second column
df[df[,2] == "X",2]<-"A"
df[df[,2] == "Y",2]<-"B"
df[df[,2] == "Z",2]<-"C"

# Add column with score of choice (3rd) and add column with score of either loss/win/draw (4th)
for (i in 1:length(df[,1])) { 
  
  ## ROCK ##
  if (df[i,2] == "A") {
    
    # Fill choice column with score 1
    df[i,3]<-1
    
    # Fill result column with either loss or win
    if (df$V1[i] == "B") {
      df[i,4]<-0 # LOSS
    } else if (df$V1[i] == "C") {
      df[i,4]<-6 # WIN
    }
  } 
  
  ## PAPER ##
  else if (df[i,2] == "B") {
    
    # Fill choice column with score 2
    df[i,3]<-2
    
    # Fill result column with either loss or win
    if (df$V1[i] == "A") {
      df[i,4]<-6 # WIN
    } else if (df$V1[i] == "C") {
      df[i,4]<-0 # LOSS
    }
  } 
  
  ## SCISSORS ##
  else { # I choose Scissors
    
    # Fill result column with either loss or win
    df[i,3]<-3
    
    # Fill result column with either loss or win
    if (df$V1[i] == "A") {
      df[i,4]<-0 # LOSS
    } else if (df$V1[i] == "B") {
      df[i,4]<-6 # WIN
    }
  }
  
  # Fill result column with every possible draw
  if (df$V1[i] == df[i,2][i]) { 
    df[i,4]<-3
  }
}

# Finally add column with total round score
df[,5]<-df[,3]+df[,4]

# Change column names
colnames(df)<-c("Player1", "Player2", "Choice", "Result", "Score")

# Total score if guide is respected
total_score<-sum(df$Score)

####### PART 2 OF ADVENT CALENDAR EXERCISE #######
rm(list=ls())

# Load data set
df<-read.table("rock_paper_scissors.txt", header=FALSE)

# Add column with score of either loss/win/draw (3rd)
df[df[,2] == "X",3]<-0
df[df[,2] == "Y",3]<-3
df[df[,2] == "Z",3]<-6

# Add column with the move needed to respect the result of each round (4th)
# The for loop goes through the each move done by 1st player
for (i in 1:length(df[,1])) {
  
  ## ROCK ##
  if (df[i,1] == "A") {
    
    if (df[i,2] == "X") {df[i,4]<-"C"} else if (df[i,2] == "Y") {df[i,4]<-"A"} 
    else {df[i,4]<-"B"}
    
  }
  
  ## PAPER ##
  if (df[i,1] == "B") {
    
    if (df[i,2] == "X") {df[i,4]<-"A"} else if (df[i,2] == "Y") {df[i,4]<-"B"} 
    else {df[i,4]<-"C"}
    
  }
  
  ## SCISSORS ##
  if (df[i,1] == "C") {
    
    if (df[i,2] == "X") {df[i,4]<-"B"} else if (df[i,2] == "Y") {df[i,4]<-"C"} 
    else {df[i,4]<-"A"}
    
  }
}

# Add column with result of each choice (5th)
df[df$V4 == "A",5]<-1
df[df$V4 == "B",5]<-2
df[df$V4 == "C",5]<-3

# Finally add column with total round score
df[,6]<-df[,3]+df[,5]

# Change column names
colnames(df)<-c("Player1", "MyResult", "ResultChoice", "ChoiceMade", "ChoiceScore", "Score")

# Total score if guide is respected
total_score<-sum(df$Score)