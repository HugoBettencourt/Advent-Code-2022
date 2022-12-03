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
  
  # Find similarities
  for (j in 1:middle) {
    if (grepl(substring(df$compartiment1[i], j, j), df$compartiment2[i]) == TRUE) {
      df$both[i]<-substring(df$compartiment1[i], j, j)
    } 
  }
}

