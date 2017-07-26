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