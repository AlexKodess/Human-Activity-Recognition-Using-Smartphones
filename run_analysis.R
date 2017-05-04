library(tidyverse)
library(data.table)

rm(list = ls())

# This is where the Samsung data set is stored
# setwd("~/Documents/Coursera/Data_Science_Johns_Hopkins/3_Getting_and_Cleaning_Data/project")

# We now read the file features.txt, which
# contains the names of all the 561 parameters 
# which will become te names of the columns in tr_set

# 4. Appropriately labels the data set with descriptive variable names.

param_names <- read_table2(
                                file = "UCI HAR Dataset/features.txt", 
                                col_types = "ic", 
                                col_names = c("id", "par_name")
                                )

training_set <- read_table2(
                                file = "UCI HAR Dataset/train/X_train.txt", 
                                col_names = param_names$par_name
                                )


test_set <- read_table2(
                                file = "UCI HAR Dataset/test/X_test.txt",
                                col_names = param_names$par_name
                                )

rm(param_names)

# bind columns
# we read activity labels from y_test.txt and y_train.txt
# and add a new column to the corresponding data sets
# we also read subject codes (1 through 30) from 
# subject_train.txt and subject_test.txt


##############################################################
train_set_label <- read_table2(
                                file = "UCI HAR Dataset/train/y_train.txt", 
                                col_types = "i",
                                col_names = "label"
)

train_set_subject <- read_table2(
                                file = "UCI HAR Dataset/train/subject_train.txt", 
                                col_types = "i",
                                col_names = "subject"
                                )

# 1) Merges the training and the test sets to create one data set

training_set <- training_set %>% 
                bind_cols(train_set_label) %>% 
                bind_cols(train_set_subject)

rm(train_set_label)
rm(train_set_subject)

##############################################################

test_set_label <- read_table2(
                                file = "UCI HAR Dataset/test/y_test.txt", 
                                col_types = "i",
                                col_names = "label"
                                )

test_set_subject <- read_table2(
                                file = "UCI HAR Dataset/test/subject_test.txt", 
                                col_types = "i",
                                col_names = "subject"
                                )

# 1) Merges the training and the test sets to create one data set

test_set <- test_set %>% 
            bind_cols(test_set_label) %>%
            bind_cols(test_set_subject)

rm(test_set_label)
rm(test_set_subject)

##############################################################


# bind rows, i.e. append rows of test_set to training_set
output <- bind_rows(training_set, test_set)

rm(training_set)
rm(test_set)

# 2) Extracts only the measurements on the mean and standard 
# deviation for each measurement.

# 3) Uses descriptive activity names to name the activities in the data set

output <- output %>%
                select(
                        matches("[Mm]ean|std|label|subject")
                ) %>%
                mutate(
                        activity = if_else(label == 1, "walking",
                                   if_else(label == 2, "walking_upstairs",
                                   if_else(label == 3, "walking_downstairs",
                                   if_else(label == 4, "sitting",
                                   if_else(label == 5, "standing","laying")))))

                        )

final_output <- output %>%
                select(-label) %>%
                group_by(subject, activity) %>%
                summarise_all(mean)

rm(output)

write.table(
                final_output, 
                file = "final_output.txt", 
                quote = FALSE,
                row.names = FALSE
                )