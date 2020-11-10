# Test of and/or filtering
df1 <- data.frame(person = c("A", "B", "C", "D"), rating = c(3, 3, 4, 5), sentence = "s1")
df2 <- data.frame(person = c("A", "C", "D", "B", "E"), rating = c(1, 1, 1, 2, 1), sentence = "s2")
df3 <- data.frame(person = c("A", "B"), rating = c(5, 5), sentence = "s3")
df4 <- data.frame(person = c("C", "D", "A"), rating = c(1, 2, 5), sentence = "s4")

dataList <- list(df1, df2, df3, df4)
dataFrame <- bind_rows(dataList)

joinTypes <- c("or", "and", "or") # "or" is for joining sentence 2; "and" is for joining sentence 3 to the combination of sentence 1 and sentence 2

dat <- dataList[[1]] # initialize with the first sentence df
for(i in 2:length(dataList)){
  newDat <- dataList[[i]]
  joinType <- joinTypes[i-1]
  if(joinType == "or"){ # if it's an "or" join, we keep all of the rows
    outDat <- bind_rows(dat, newDat)
  }else{ # if it's an "and" join, we only keep people who appear in both data frames
    outDat <- bind_rows(dat, newDat) %>%
      group_by(person) %>%
      filter(person %in% newDat$person) %>% # only keep the people who appear in both the new data and the previous data
      ungroup()
  }
  dat <<- outDat # update dat with the new sentence data, joined by "or" or "and"
}

