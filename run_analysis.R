aggregate_data <- function(df) {
  factor1 <- df$activity
  factor2 <- df$subject
  factors <- list(factor1, factor2)
  split_df <- split(df, factors)
  column_names <- colnames(df)
  final_df <- data.frame(matrix(nrow = 0, ncol = length(column_names)))
  colnames(final_df) <- column_names
  for (f in split_df) {
    temp_df <- cbind(f$activity[1], f$subject[1], as.data.frame(t(colMeans(f[, 3:ncol(f)]))))
    colnames(temp_df) <- column_names
    final_df <- rbind(final_df, temp_df)
  }
  return(final_df)
}

combine_data <- function() {
  train_set_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
  test_set_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
  names(train_set_subject) <- "subject"
  names(test_set_subject) <- "subject"
  
  activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
  colnames(activities) <- c("activity_number", "activity")
  train_set_y <- read.table("./UCI HAR Dataset/train/y_train.txt")
  test_set_y <- read.table("./UCI HAR Dataset/test/y_test.txt")
  names(train_set_y) <- "activity_number"
  names(test_set_y) <- "activity_number"
  
  col_names<- readLines("./UCI HAR Dataset/features.txt")
  col_names <- gsub("^[0-9]+ ", "", col_names)
  test_set_x <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
  train_set_x <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
  
  colnames(train_set_x) <- col_names
  colnames(test_set_x) <- col_names
  
  
  train_set <- cbind(train_set_subject, train_set_y, train_set_x)
  test_set <- cbind(test_set_subject, test_set_y, test_set_x)
  full_set <- rbind(train_set, test_set)
  required_cols <- grep("subject|activity_number|mean()|std()", names(full_set), value = TRUE)
  full_set <- full_set[, required_cols]
  rownames(full_set) <- NULL
  full_set <- merge(activities, full_set, by = "activity_number")
  full_set <- full_set[, -1]
  full_set$activity <- gsub("_", " ", full_set$activity)
  
  return(full_set)
}