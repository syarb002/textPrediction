# Frequency Table Builder for Capstone Project
# 
# Grabs 1,000 documents at a time, tokenizes them, then summarizes by
# frequency.  Limits analysis so that the system memory isn't exceeded.

i <- 1  # Set this to wherever you left off if processing interrupted
while (i < 333667) {
  if (i == 1) {
    dfm4G <- dfm(sampleCorp[1:1000], ngrams = 4, concatenator = " ")
  } else {
    dfm4G <- dfm(sampleCorp[i:(i+999)], ngrams = 4, concatenator = " ")
  }
  temp <- colSums(as.matrix(quanteda::as.DocumentTermMatrix(dfm4G)))
  temp <- data.table(names(temp), temp)
  if (i == 1) {
    freq4 <- temp
  } else {
    freq4 <- data.table(setnames(summarize(group_by(full_join(freq4, temp, by = "V1"), V1), sum(temp.x, temp.y)), c("V1", "temp")))
    freq4[is.na(temp) == TRUE]$temp <- 1
  }
  i <- i + 1000
  print(i)
}
save.image()
