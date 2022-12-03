library(readr)

rm(list=ls())

# Load data source
df<-read.table("input.txt", header=FALSE, blank.lines.skip = FALSE)

# Number of elves
n_elfs<-sum(is.na(df$V1), na.rm = TRUE)+1

# Find row number where each NA is found
rows_elves<-which(is.na(df$V1), arr.ind=TRUE)

# Attach onto elf vector the ordinal number associated to each elf
# Attach onto cal vector the sum of the calories associated to each elf
elf<-vector()
cal<-vector()
for (e in rows_elves) {
  if (e == rows_elves[1]) {
    a<-1
    elf<-append(elf, a)
    cal<-append(cal, sum(df[1:(e-1),]))
  } else {
    elf<-append(elf, a)
    cal<-append(cal, sum(df[b:(e-1),]))
  }
  a<-a+1
  b<-e+1
}

elf<-append(elf, n_elfs)
cal<-append(cal, sum(rows_elves[length(rows_elves)]+1:length(df)))

# Create data frame
df2<-data.frame("elf"=elf, "cal"=cal)
df3<-df2[order(-df2$cal),]
sum(df3[1:3,2])