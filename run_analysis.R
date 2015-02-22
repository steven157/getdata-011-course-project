Run_Analysis <- function() {
  
  current <- getwd()
  setwd("UCI HAR Dataset")
  current2 <- getwd()
  setwd("train")
  subject_train <- read.table("subject_train.txt")
  train <- read.table("X_train.txt")
  activity_train <- read.table("y_train.txt")
  data_train <- data.frame(subject_train,activity_train,train)
  setwd(current2)
  setwd("test")
  subject_test <- read.table("subject_test.txt")
  test <- read.table("X_test.txt")
  activity_test <- read.table("y_test.txt")
  data_test <- data.frame(subject_test,activity_test,test)
  raw_data <- rbind(data_train,data_test)
  
  setwd(current2)
  raw_name <- as.vector(read.table("features.txt")[,2])
  col_label <- c("subject","activity",raw_name)
  check <- grepl("mean\\(\\)|std\\(\\)",col_label)
  extracted_data <- data.frame(raw_data[,1:2],raw_data[,check])
  
  activity_labels <- as.vector(read.table("activity_labels.txt")[,2])
  arrangement <- order(extracted_data[,1],extracted_data[,2])
  sorted_data <- extracted_data[arrangement,]
  hold <- rep("a",nrow(sorted_data))
  for(i in 1:nrow(sorted_data)){
    hold[i] <- activity_labels[sorted_data[i,2]]
  }
  factored_data <- data.frame(sorted_data[,1],hold,sorted_data[3:ncol(sorted_data)])
  
  factored_name <- col_label[check]
  colnames(factored_data) <- c("subject","activity",factored_name)
  
  total_subject <- length(unique(factored_data[,1]))
  total_activity <- length(activity_labels)
  subject <- vector()
  row <- total_subject*total_activity
  col <- sum(check)
  tidy_matrix <- matrix(nrow=row,ncol=col)
  for(i in 1:total_subject){
    subject <- cbind(subject,rep(i,6))
    for(j in 1:total_activity){
      temp <- data.frame()
      select <- (factored_data[,1]==i & factored_data[,2]==activity_labels[j])
      temp <- factored_data[select,]
      for(k in 1:ncol(tidy_matrix)){
        x <- 6*(i-1)+j
        tidy_matrix[x,k] <- mean(temp[,k+2])
      }
    }
  }
  subject <- as.vector(subject)
  activity <- rep(activity_labels,total_subject)
  tidy_name <- paste("average_of_",factored_name,sep="")
  tidy_data <- data.frame(subject,activity,tidy_matrix)
  colnames(tidy_data) <- c("subject","activity",tidy_name)
  library(reshape2)
  data_melt <- melt(tidy_data,id=c("subject","activity"),measure.vars=tidy_name)
  setwd(current)
  write.table(data_melt,"tidy data.txt",row.names=FALSE,quote=FALSE)
}