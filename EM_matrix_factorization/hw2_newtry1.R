#rm(list = ls())
# install.packages("psych")
# library("psych")

df <- read.csv("ratings.csv",
               header=T)

#number of users
N <- max(df$user_id)

#number of movies
M <- max(df$movie_id)

user_rate_movie <- rep(NA,N) #make an empty vec of length users (for movies)
movie_by_user <- rep(NA,M) #make an empty vec of length movies (for users)

#count the number of movies rated by given user and number of ratings received by given movie
for(i in 1:N){
  user_rate_movie[i] <- dim(df[df$user_id == i,])[1]
}
for(i in 1:M){
  movie_by_user[i] <- dim(df[df$movie_id == i,])[1]
}

##create two lists for users
#first list contains information about the movies each user rated
#second list contains the ratings of those movies 
given_user_movie_info <- list()
given_user_movie_rating <- list()

##create two lists for movies
#first list contains information about the users who watched each movie
#second list contains the ratings those users gave those movies
given_movie_user_info <- list()
given_movie_user_rating <- list()


for(i in 1:N){
  given_user_movie_info[[i]] <- rep(0, user_rate_movie[i])
  given_user_movie_rating[[i]] <- rep(0, user_rate_movie[i])
}
for(i in 1:M){
  given_movie_user_info[[i]] <- rep(0, movie_by_user[i])
  given_movie_user_rating[[i]] <- rep(0, movie_by_user[i])
}

#now filling in the lists
for(i in 1:N){
  given_user_movie_info[[i]] <- df$movie_id[df$user_id == i]
  given_user_movie_rating[[i]] <- df$rating[df$user_id == i]
  
}
for(i in 1:M){
  given_movie_user_info[[i]] <- df$user_id[df$movie_id == i]
  given_movie_user_rating[[i]] <- df$rating[df$movie_id == i]
}

cnt_i <- rep(1, N)
cnt_j <- rep(1, M)

for(i in 1:dim(df)[1])
{
  given_user_movie_info[[df[i,1]]][cnt_i[df[i,1]]] <- df[i,2]
  given_user_movie_rating[[ df[i,1] ]][cnt_i[ df[i,1] ] ] <- df[i,3]
  cnt_i[ df[i,1] ] = cnt_i[ df[i,1] ] + 1
  
  given_movie_user_info[[ df[i,2] ]][cnt_j[ df[i,2] ] ] <- df[i,1]
  given_movie_user_rating[[ df[i,2] ]][cnt_j[ df[i,2] ] ] = df[i,3]
  cnt_j[ df[i,2] ] = cnt_j[ df[i,2] ] + 1
}


###########################
###### EM Algorithm #######
###########################
#given information
d <- 10
c <- 1/10
sigma_sq <- 1/4

#initialize v_j and draw from prior
v_j = matrix(0,nrow=M,ncol=d)
for(i in 1:N){ 
  v_j[i,] <- rnorm(10,0,sqrt(0.1))
}

#posterior parameters of U
#vector means
mu_ui <- matrix(0,nrow = N,ncol = d) #one for each user
#matrix variances
sigma_ui <- array(0, dim = c(d,d,N)) #one for each user

#iterations
max_iter <- 50

#create arrays for mu_ui, sigma_ui and v_j to save each iteration values
out_v_j = array(0, dim=c(M,d,max_iter))
out_mu_ui = array(0, dim=c(N,d,max_iter))
out_sigma_ui = array(0, dim=c(d,d,N,max_iter))

#objective function
L_V = rep(0,max_iter)

#running the algorithm
em <- function(start = 0){
  # Track time
  start <- Sys.time()
for(iter in 1:max_iter){
  #############
  for(i in 1:N){
    vvj <- matrix(0,nrow = d, ncol = d) #initialize vvj
    rvj <- rep(0, times = d) #initialize rvj
    #for the jth movie rated by the ith user, compute vj (movie attribute) and rvj (associated rating)
    for(j in 1:user_rate_movie[i]){
      vj <- v_j[given_user_movie_info[[i]][j],]
      rvj <- rvj + given_user_movie_rating[[i]][j]*vj
      vvt <- vj %*% t(vj)
      vvj <- vvj + vvt
    }
    sigma_ui[,,i] <- solve((1/c)*diag(d) + (1/sigma_sq)*vvj)
    mu_ui[i,] <- (1/sigma_sq)*sigma_ui[,,i]%*%rvj
  }
  #############
  for(j in 1:M){	
    term_1 <- matrix(0,nrow=10,ncol=10)
    term_2 <- rep(0,10)
    
    for(i in 1:movie_by_user[j]){
      term_1 <- term_1 + sigma_ui[,, given_movie_user_info[[j]][i]]+(mu_ui[given_movie_user_info[[j]][i], ]%*%t(mu_ui[ given_movie_user_info[[j]][i],]))
      term_2 <- term_2 + given_movie_user_rating[[j]][i]*mu_ui[ given_movie_user_info[[j]][i],]
    }
    term_1	<- ((sigma_sq/c)*diag(d)) + term_1
    v_j[j,]	= solve(term_1,term_2)
  }
  out_v_j[,,iter] <- v_j
  out_mu_ui[,,iter] <- mu_ui
  out_sigma_ui[,,,iter] <- sigma_ui
  #############
  
  lv_1 <- 0
  for(i in 1:N){
    for(j in 1:user_rate_movie[i]){
      vj <- v_j[given_user_movie_info[[i]][j],]
      mu_vj <- t(vj) %*% mu_ui[i,]
      lv_1 <- lv_1 + (given_user_movie_rating[[i]][j])^2 - 2*given_user_movie_rating[[i]][j]*mu_vj + mu_vj^2 + (t(vj) %*% sigma_ui[,,i] %*% vj)
    }
  }
  lv_1 <- lv_1/(-2*sigma_sq)
  
  for(j in 1:M){ 
  lv_2 <- ((t(v_j[j,]) %*% v_j[j,])/(-2*c))
  }
  
  lv <- lv_1 + lv_2
  
  L_V[iter] <- lv
  print(iter)
}
  # Track time
  end <- Sys.time()
  print(end - start)
  return(L_V = L_V)
}

#testing
t1 <- em(start = 0)
plot(t1[3:50],
     type = 'l')

