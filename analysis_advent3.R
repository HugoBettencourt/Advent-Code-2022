library(stringr)

####### PART 1 #######
rm(list=ls())
# Load data frame
df<-read.table("rucksacks.txt", header=FALSE)

# Change name of column
colnames(df)<-c("Rucksack")

# Split each rucksack into the two compartiments
for (i in 1:length(df[,1])) {
  
  # Calculate total number of items in a ruksack and the middle index (always the same)
  n_items<-nchar(df[i,1])
  middle<-n_items/2
  
  # Separate the rucksack into both compartiments
  df$compartiment1[i]<-substring(df$Rucksack[i], 1, middle)
  df$compartiment2[i]<-substring(df$Rucksack[i], (middle+1), n_items)
  
  # Find similarities and add it in the fourth column
  for (j in 1:middle) {
    if (grepl(substring(df$compartiment1[i], j, j), df$compartiment2[i]) == TRUE) {
      df$both[i]<-substring(df$compartiment1[i], j, j)
    } 
  }
  
  # Add score of each rucksack in the fifth column
  if (str_detect(df$both[i],"[[:upper:]]") == FALSE) {
    df$score[i]<-match(df$both[i], letters)
  } else {
    df$score[i]<-match(tolower(df$both[i]), letters)+26
  }
}

####### PART 2 #######
rm(list=ls())
# Load data frame
df<-read.table("rucksacks.txt", header=FALSE)

# Change name of column
colnames(df)<-c("Rucksack")

# Go through each group by creating a vector with the initialize member index
ind<-seq(1,length(df[,1]),3)

a<-1
group<-vector()
badge<-vector()
score<-vector()
for (i in ind[1:10]) {
  n_items<-nchar(df[i,1])
  
  for (j in 1:n_items) {
    if (grepl(substring(df[i,1], j, j), df[i+1,1]) == TRUE) {
      if (grepl(substring(df[i,1], j, j), df[i+2,1]) == TRUE) {
        group<-append(group, a)
        badge<-append(badge, substring(df[i,1], j, j))
        a<-a+1
        
        if (str_detect(substring(df[i,1], j, j),"[[:upper:]]") == FALSE) {
          score<-append(score, match(substring(df[i,1], j, j), letters))
        } else {
          score<-append(score, match(tolower(substring(df[i,1], j, j)), letters)+26)
        }
      }
    } 
  }
}

df2<-data.frame("group"=group, "badge"=badge, "score"=score)
