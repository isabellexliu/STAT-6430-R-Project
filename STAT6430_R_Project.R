moviereviews_df <-read.csv(text=getURL("https://raw.githubusercontent.com/rohanbapat/STAT-6430-R-Project/master/reviews.txt"), header=F, sep ="\t")
colnames(moviereviews_df) <- c('ReviewerID','MovieID','Rating','TimeStampUTC')
moviereviews_df$TimeStampLT<-as.POSIXct(moviereviews_df$TimeStampUTC,origin = "1970-01-01")

#-------------------------------------------------------------------------------------------------------------------------

# Problem 1

# Number of movies by movie rating
rating_summary_df <- moviereviews_df%>%group_by(Rating)%>%summarise(countRating = length(Rating))

# Percentage by movie rating
rating_summary_df$perRating <- rating_summary_df$countRating/sum(rating_summary_df$countRating)

#-------------------------------------------------------------------------------------------------------------------------

# Problem 2

# Number of reviews provided by each reviewer 
reviewercount_summary_df <- moviereviews_df%>%group_by(ReviewerID)%>%summarise(ratingProvided = length(Rating))

# Top 10 reviewers
top10_reviewers <- head(reviewercount_summary_df[order(-reviewercount_summary_df$ratingProvided),],10)

#-------------------------------------------------------------------------------------------------------------------------

# Problem 3

# Average ratings provided at user level
reviewermean_summary_df <- moviereviews_df%>%group_by(ReviewerID)%>%summarise(ratingProvided = mean(Rating))

# Metrics for calculating confidence intervals
mean_overall_reviewers <- mean(reviewermean_summary_df$ratingProvided)
sd_overall_reviewers <- sd(reviewermean_summary_df$ratingProvided)
n_overall_reviewers <- nrow(reviewermean_summary_df)
t_overall_reviewers <- qt(0.975,n_overall_reviewers-1)

# Upper and lower limits of 95% confidence interval for all reviewers
upper_lt_overall_reviewers <- mean_overall_reviewers + t_overall_reviewers*sd_overall_reviewers/sqrt(n_overall_reviewers)
lower_lt_overall_reviewers <- mean_overall_reviewers - t_overall_reviewers*sd_overall_reviewers/sqrt(n_overall_reviewers)

# Average ratings provided by top 10 reviewers
top10_reviewermean_summary_df <- reviewer_summary_df[reviewer_summary_df$ReviewerID %in% top_10_reviewers$ReviewerID,]

# Metrics for calculating confidence intervals
mean_top10_reviewers <- mean(top10_reviewermean_summary_df$ratingProvided)
sd_top10_reviewers <- sd(top10_reviewermean_summary_df$ratingProvided)
n_top10_reviewers <- nrow(top10_reviewermean_summary_df)
t_top10_reviewers <- qt(0.975,n_top10_reviewers-1)

# Upper and lower limits of 95% confidence interval for all reviewers
upper_lt_top10_reviewers <- mean_top10_reviewers + t_top10_reviewers*sd_top10_reviewers/sqrt(n_top10_reviewers)
lower_lt_top10_reviewers <- mean_top10_reviewers - t_top10_reviewers*sd_top10_reviewers/sqrt(n_top10_reviewers)

#-----------------------------------------------------------------------------------------------------
#Problem 4

#import genres.txt to get Movie Title from MovieID
genres_df <-read.csv(text=getURL("https://raw.githubusercontent.com/rohanbapat/STAT-6430-R-Project/master/genres.txt"), header=F, sep ="|")
colnames(genres_df) <- c('ReviewerID','MovieID','Rating','TimeStampUTC')

#NEED TO FIGURE OUT HOW TO GET RELEASEDATE RIGHT AFTER MOVIE TITLE WHICH IS NOT DELIMETED BY A |
colnames(genres_df) <- c('MovieID','MovieTitle','ReleaseDate','VideoReleaseDate','IMDbURL','unknown','Action','Adventure','Animation',
                         'Childrens','Comedy','Crime','Documentary','Drama','Fantasy','FilmNoir','Horror','Musical',
                         'Mystery','Romance','SciFi','Thriller','War','Western')

#library(stringr)

#separate1 <- for i in row
#              int(i) >= 1 then genres_df$VideoReleaseDate = i
              
#separate(genres_df$MovieTitle)
#str_split_fixed(genres_df$MovieTitle, " (", 2)

movie_genres_merged <- merge(moviereviews_df, genres_df,by="MovieID")



# Number of movies reviewed 
MovieTitle_summary_df <- movie_genres_merged%>%group_by(MovieTitle)%>%summarise(TimesReviewed = length(MovieID))

# Top 10 movies reviewed by MovieTitle
top10_movies_reviewed <- head(MovieTitle_summary_df[order(-MovieTitle_summary_df$TimesReviewed),],10)

#-----------------------------------------------------------------------------------------------------
#Problem 5

#specifying only genres - excluding unknown
genres_only_df <- movie_genres_merged[, 11:28]

genres_only_df%>%group_by(MovieTitle)%>%summarise(TimesReviewed = length(MovieID))

genres_only_df_adjusted <- genres_only_df(countAction = sum(movie_genres_merged$Action == 1))


#sum(test==1)
genres_only_df_adjusted <- data.frame(sum(movie_genres_merged$Action == 1),
                                      sum(movie_genres_merged$Adventure == 1),
                                      sum(movie_genres_merged$Animation == 1),
                                      sum(movie_genres_merged$Childrens == 1),
                                      sum(movie_genres_merged$Comedy == 1),
                                      sum(movie_genres_merged$Crime == 1),
                                      sum(movie_genres_merged$Documentary == 1),
                                      sum(movie_genres_merged$Drama == 1),
                                      sum(movie_genres_merged$Fantasy == 1),
                                      sum(movie_genres_merged$FilmNoir == 1),
                                      sum(movie_genres_merged$Horror == 1),
                                      sum(movie_genres_merged$Musical == 1),
                                      sum(movie_genres_merged$Mystery == 1),
                                      sum(movie_genres_merged$Romance == 1),
                                      sum(movie_genres_merged$SciFi == 1),
                                      sum(movie_genres_merged$Thriller == 1),
                                      sum(movie_genres_merged$War == 1),
                                      sum(movie_genres_merged$Western == 1))

colnames(genres_only_df_adjusted) <- c('Action','Adventure','Animation',
                         'Childrens','Comedy','Crime','Documentary','Drama','Fantasy','FilmNoir','Horror','Musical',
                         'Mystery','Romance','SciFi','Thriller','War','Western')

#Ordering genres by count
genres_only_df_adjusted <- sort(genres_only_df_adjusted)

#ANSWER
#Genre that occured MOST = Drama
#Genre that occured LEAST = Documentary
                        

#-----------------------------------------Problem 6--------------------------------------------------

#adding new column that identifies number of genres for each movie
movie_genres_merged$CountOfGenres <- rowSums(movie_genres_merged[10:28])

#number of movies with two or more genres
sum(movie_genres_merged$CountOfGenres >= 2)
nrow(movie_genres_merged[movie_genres_merged$CountOfGenres >= 2,])
#69938

#number of movies total
nrow(movie_genres_merged)
#100000

#percentage of reviews that invovled movies classified in at least two genres 
nrow(movie_genres_merged[movie_genres_merged$CountOfGenres >= 2,])/nrow(movie_genres_merged)
#0.69938
#ANSWER 69.9938% of all reviews involved at least two genres

#------------------------------------------------------------------------------------------

# Problem 7

# Read in reviewers.txt as a csv file, and name the columns
reviewers_df <- read.csv(text = getURL("https://github.com/rohanbapat/STAT-6430-R-Project/blob/master/reviewers.txt"), header = F, sep = "|")
colnames(reviewers_df) <- c('ReviewerID', 'Age', 'Gender', 'Occupation', 'Zipcode')

# Merge moviereviews and reviewers together
reviews_reviewers <- merge(moviereviews_df, reviewers_df, by = "ReviewerID")

# Split the dataframe by gender
gendermean_summary_df <- split(reviews_reviewers, reviews_reviewers$Gender)

# Metrics for calculating confidence interval (male)
mean_m_reviewers <- mean(gendermean_summary_df$M$Rating)
n_m_reviewers <- nrow(gendermean_summary_df$M)
t_m_reviewers <- qt(0.975, n_m_reviewers - 1)
sd_m_reviewers <- sd(gendermean_summary_df$M$Rating)
# Upper and lower limits of 95% confidence interval for male reviewers
upper_lt_m_reviewers <- mean_m_reviewers + t_m_reviewers * sd_m_reviewers / sqrt(n_m_reviewers)
lower_lt_m_reviewers <- mean_m_reviewers - t_m_reviewers * sd_m_reviewers / sqrt(n_m_reviewers)
c(lower_lt_m_reviewers, upper_lt_m_reviewers)
# Answer: 3.521309 3.537269

# Metrics for calculating confidence interval (female)
mean_f_reviewers <- mean(gendermean_summary_df$F$Rating)
n_f_reviewers <- nrow(gendermean_summary_df$F)
t_f_reviewers <- qt(0.975, n_f_reviewers - 1)
sd_f_reviewers <- sd(gendermean_summary_df$F$Rating)
# Upper and lower limits of 95% confidence interval for female reviewers
upper_lt_f_reviewers <- mean_f_reviewers + t_f_reviewers * sd_f_reviewers / sqrt(n_f_reviewers)
lower_lt_f_reviewers <- mean_f_reviewers - t_f_reviewers * sd_f_reviewers / sqrt(n_f_reviewers)
c(lower_lt_f_reviewers, upper_lt_f_reviewers)
# Answer: 3.517202 3.545813

#------------------------------------------------------------------------------------------

# Problem 8

zipcodes_df <- read.csv(text = getURL("https://github.com/rohanbapat/STAT-6430-R-Project/blob/master/zipcodes.csv"), header = T)

# Merge reviews_reviewers and zipcodes together
reviews_reviewers_zipcodes <- merge(reviews_reviewers, zipcodes_df, by = "Zipcode")

# Find the number of reviews for each state, and order them in descending orders
statecount_summary_df <- data.frame(table(reviews_reviewers_zipcodes$State))
statecount_summary_df <- statecount_summary_df[order(-statecount_summary_df$Freq),]

# The states with top 5 most reviews
statecount_summary_df$Var1[1:5]
# Answer: CA MN NY IL TX

#------------------------------------------------------------------------------------------

# Problem 9

# Find the number of reviews for each movie, and order them in ascending orders
mvidcount_summary_df <- data.frame(table(moviereviews_df$MovieID))
mvidcount_summary_df <- mvidcount_summary_df[order(mvidcount_summary_df$Freq),]

# Find the number of movies with 1-20 reviews
reviewscount_summary_df <- data.frame(table(mvidcount_summary_df$Freq))
reviewscount_summary_df <- reviewscount_summary_df$Freq[1:20]

# Create a list to store all percentage of movies that have 1-20 reviews
mvreviews_pctg <- list()
for (i in 1:20){
  mvreviews_pctg <- c(mvreviews_pctg, reviewscount_summary_df[i] / length(mvidcount_summary_df$Var1))
}
mvreviews_pctg
# Answer: 0.08382878 0.04042806  0.03567182 0.03804994  0.03032105 (1-5 reviews)
#         0.02318668 0.02615933  0.01783591 0.0196195   0.0196195 (6-10)
#         0.01189061 0.01664685  0.01486326 0.008323424 0.01307967 (11-15)
#         0.01129608 0.005945303 0.01426873 0.01070155  0.007134364 (16-20)

# In Percentage: 8.382878% 4.042806%  3.567182% 3.804994%  3.032105% (1-5 reviews)
#                2.318668% 2.615933%  1.783591% 1.96195%   1.96195% (6-10)
#                1.189061% 1.664685%  1.486326% 0.8323424% 1.307967% (11-15)
#                1.129608% 0.5945303% 1.426873% 1.070155%  0.7134364% (16-20)

#------------------------------------------------------------------------------------------

# Problem 10

# Create a dataframe to store all genres and their corresponding average ratings
genre_rating_df <- as_data_frame(matrix(NA, nrow = 19, ncol = 2))
for (i in 10:28){
  genre_rating <- split(movie_genres_merged, movie_genres_merged[[i]])
  genremean_rating <- mean(genre_rating$`1`$Rating)
  genre_rating_df[i-9, 1] <- colnames(movie_genres_merged)[i]
  genre_rating_df[i-9, 2] <- genremean_rating
}

# Sort the dataframe based on ratings, and find the genre with highest and lowest ratings
genre_rating_df <- genre_rating_df[order(genre_rating_df$V2),]
highest_rating_genre <- genre_rating_df$V1[19]
lowest_rating_genre <- genre_rating_df$V1[1]
highest_rating_genre
lowest_rating_genre
# Answer: FilmNoir
#         unknown

#-----------------------------------------------------------------------------------------

# Problem 11

# Merge moviereviews, genres and reviewers together
movie_genres_age <- merge(movie_genres_merged, reviewers_df, by = "ReviewerID")
movie_genres_age

# Divide the dataframe into two parts based on age
is_under_30 <- subset(movie_genres_age, movie_genres_age$Age <= 30)
above_30 <- subset(movie_genres_age, movie_genres_age$Age > 30)

# For reviewers age 30 and under
genre_rating_u30_df <- as_data_frame(matrix(NA, nrow = 19, ncol = 2))
for (i in 10:28){
  genre_rating_u30 <- split(is_under_30, is_under_30[[i]])
  genremean_rating_u30 <- mean(genre_rating_u30$`1`$Rating)
  genre_rating_u30_df[i-9, 1] <- colnames(is_under_30)[i]
  genre_rating_u30_df[i-9, 2] <- genremean_rating_u30
}
genre_rating_u30_df <- genre_rating_u30_df[order(genre_rating_u30_df$V2),]
highest_rating_genre_u30 <- genre_rating_u30_df$V1[19]
lowest_rating_genre_u30 <- genre_rating_u30_df$V1[1]
highest_rating_genre_u30
lowest_rating_genre_u30
# Answer: FilmNoir
#         Fantasy

# For reviewers over 30
genre_rating_a30_df <- as_data_frame(matrix(NA, nrow = 19, ncol = 2))
for (i in 10:28){
  genre_rating_a30 <- split(above_30, above_30[[i]])
  genremean_rating_a30 <- mean(genre_rating_a30$`1`$Rating)
  genre_rating_a30_df[i-9, 1] <- colnames(above_30)[i]
  genre_rating_a30_df[i-9, 2] <- genremean_rating_a30
}
genre_rating_a30_df <- genre_rating_a30_df[order(genre_rating_a30_df$V2),]
highest_rating_genre_a30 <- genre_rating_a30_df$V1[19]
lowest_rating_genre_a30 <- genre_rating_a30_df$V1[1]
highest_rating_genre_a30
lowest_rating_genre_a30
# Answer: FilmNoir
#         unknown
