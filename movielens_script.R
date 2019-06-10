## Data Load and libraries

#Load MovieLens dataset (Code copied from Edx project instructions)

#load packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

## Data cleansing and error checking

#check for NA's

sum(is.na(edx)) 

#view summaries of each field for 0's, outliers, and anomalies

summary(edx$userId)
summary(edx$rating)
summary(edx$movieId)
data.frame(table(edx$genres))

#create dummy variables for genres

genres <- cSplit(edx[6], 'genres', sep="|", type.convert=FALSE)
genres <- unique(genres[,1])

for (i in 1:19) {
  gen <- genres[i]
  index <- grep(gen, edx$genres)
  edx[index,6+i] <-1
  edx[-index,6+i] <-0
}

for (i in 1:19) {
  gen <- genres[i]
  index <- grep(gen, validation$genres)
  validation[index,6+i] <-1
  validation[-index,6+i] <-0
}

#create average movie ratings and user ratings

avgmovierating <- edx %>% group_by(movieId) %>% summarize(avgmovierating = mean(rating))
avguserrating <- edx %>% group_by(userId) %>% summarize(avguserrating = mean(rating))

edx <- merge(edx, avgmovierating, 'movieId', all.x = TRUE)
edx <- merge(edx, avguserrating, 'userId', all.x = TRUE)

validation <- merge(validation, avgmovierating, 'movieId', all.x = TRUE)
validation <- merge(validation, avguserrating, 'userId', all.x = TRUE)

#create individual average movie ratings for top 5 genreas

avguserrating_comedy <- edx[edx$V7 == 1,] %>% group_by(userId) %>% summarize(avguserrating_comedy = mean(rating))
edx <- merge(edx, avguserrating_comedy, 'userId', all.x = TRUE)
validation <- merge(validation, avguserrating_comedy, 'userId', all.x = TRUE)

avguserrating_action <- edx[edx$V8 == 1,] %>% group_by(userId) %>% summarize(avguserrating_action = mean(rating))
edx <- merge(edx, avguserrating_action, 'userId', all.x = TRUE)
validation <- merge(validation, avguserrating_action, 'userId', all.x = TRUE)

avguserrating_adventure <- edx[edx$V10 == 1,] %>% group_by(userId) %>% summarize(avguserrating_adventure = mean(rating))
edx <- merge(edx, avguserrating_adventure, 'userId', all.x = TRUE)
validation <- merge(validation, avguserrating_adventure, 'userId', all.x = TRUE)

avguserrating_drama <- edx[edx$V12 == 1,] %>% group_by(userId) %>% summarize(avguserrating_drama = mean(rating))
edx <- merge(edx, avguserrating_drama, 'userId', all.x = TRUE)
validation <- merge(validation, avguserrating_drama, 'userId', all.x = TRUE)

avguserrating_crime <- edx[edx$V13 == 1,] %>% group_by(userId) %>% summarize(avguserrating_crime = mean(rating))
edx <- merge(edx, avguserrating_crime, 'userId', all.x = TRUE)
validation <- merge(validation, avguserrating_crime, 'userId', all.x = TRUE)

#view summary of engineered features
summary(edx[,c(3,26:32)])

#remove extra tables and clear memory
rm(avgmovierating,avguserrating,avguserrating_action,avguserrating_adventure,avguserrating_comedy,avguserrating_crime, avguserrating_drama, gen, genres, i, index)
gc()

#replace NA's with means
edx$avguserrating_comedy[is.na(edx$avguserrating_comedy)] <- mean(na.omit(edx$avguserrating_comedy))
validation$avguserrating_comedy[is.na(validation$avguserrating_comedy)] <- mean(na.omit(edx$avguserrating_comedy))

edx$avguserrating_action[is.na(edx$avguserrating_action)] <- mean(na.omit(edx$avguserrating_action))
validation$avguserrating_action[is.na(validation$avguserrating_action)] <- mean(na.omit(edx$avguserrating_action))

edx$avguserrating_adventure[is.na(edx$avguserrating_adventure)] <- mean(na.omit(edx$avguserrating_adventure)) 
validation$avguserrating_adventure[is.na(validation$avguserrating_adventure)] <- mean(na.omit(edx$avguserrating_adventure))

edx$avguserrating_drama[is.na(edx$avguserrating_drama)] <- mean(na.omit(edx$avguserrating_drama))
validation$avguserrating_drama[is.na(validation$avguserrating_drama)] <- mean(na.omit(edx$avguserrating_drama))

edx$avguserrating_crime[is.na(edx$avguserrating_crime)] <- mean(na.omit(edx$avguserrating_crime))
validation$avguserrating_crime[is.na(validation$avguserrating_crime)] <- mean(na.omit(edx$avguserrating_crime))

summary(edx[,c(3,26:32)])

#fit LM model
fit.lm <- lm(rating~., data = edx[,c(3,7:32)])
summary(fit.lm)

#train RMSE
predict.lm <- predict(fit.lm, newdata = edx[,c(3,7:32)])
sqrt(mean((edx$rating - predict.lm)^2))

#remove previous fit to save memory
rm(fit.lm)
rm(predict.lm)

#fit LM model and remove non-signicant variables
fit.lm <- lm(rating~., data = edx[,c(3,7:16,18:21,23:24,26:32)])
summary(fit.lm)

#train RMSE
predict.lm <- predict(fit.lm, newdata = edx[,c(3,7:32)])
sqrt(mean((edx$rating - predict.lm)^2))

#remove previous predict to save memory
rm(predict.lm)

#final validation RMSE
predict.lm <- predict(fit.lm, newdata = validation[,c(3,7:32)])
sqrt(mean((validation$rating - predict.lm)^2))
