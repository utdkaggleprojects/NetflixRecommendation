############################################################################################
library(lsa)
#load the movie ratings data into a data frame
ratings <- read.csv("C:/Users/netapalli/Downloads/R_Projects/movieratings.csv")
ratings <- ratings[rowSums(is.na(ratings))!=(ncol(ratings)-1),]
#select all the movieid columns except the userid column from the input data frame to calculate the similarity matrix between all the movie ids
x <- ratings[,2:ncol(ratings)]

#replace all the "NA" values with 0
x[is.na(x)] <- 0

#generate the similarity matrix using "cosine" similarity measure
movie_sim <- cosine(as.matrix(x))

#To see a gist of the matrix
movie_sim

#Code for the movie recommendation system for a particular user id
Movie_rec_sys = function(user)
{
  userRatings = ratings[ratings$userid==user,]
  non_rated_movies = list()
  rated_movies = list()
  for(i in 2:ncol(userRatings))
    {
      if(is.na(userRatings[,i]))
      {
        non_rated_movies = c(non_rated_movies,colnames(userRatings)[i])
      }
    else{
      rated_movies = c(rated_movies,colnames(userRatings)[i])
    }
  }
  non_rated_movies = unlist(non_rated_movies)
  rated_movies = unlist(rated_movies)
  non_rated_pred_score = list()
  for(j in 1:length(non_rated_movies)){
    temp_sum = 0
    df = movie_sim[which(rownames(movie_sim)==non_rated_movies[j]),]
    for(i in 1:length(rated_movies)){
      temp_sum = temp_sum+ df[which(names(df)==rated_movies[i])]
    }
    weight_mat = df*ratings[ratings$userid==user,2:ncol(ratings)]
    non_rated_pred_score = c(non_rated_pred_score,rowSums(weight_mat,na.rm=T)/temp_sum)
  }
  pred_rat_mat = as.data.frame(non_rated_pred_score)
  names(pred_rat_mat) = non_rated_movies
  for(k in 1:ncol(pred_rat_mat)){
    ratings[ratings$userid==user,][which(names(ratings[ratings$userid==user,]) == names(pred_rat_mat)[k])] = pred_rat_mat[1,k]
  }
  return(ratings[ratings$userid==user,])
}

Movie_rec_sys(2)



