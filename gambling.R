#Loading the library
library(tidyverse)

#Loading the dataset
df=read_csv('bustabit.csv')
head(df)

max_score=max(df$BustedAt)

df <- df %>% 
  mutate(CashedOut = ifelse(is.na(CashedOut), BustedAt + .01, CashedOut),
         Profit = ifelse(is.na(Profit), 0, Profit),
         Losses = ifelse(Profit == 0, Bet-CashedOut, 0),
         GameWon = ifelse(Profit == 0, 0, 1),
         GameLost = ifelse(Profit == 0,1,0))

head(df)


df_clus <- df %>%
  group_by(Username) %>%
  summarize(AverageCashedOut = mean(CashedOut), 
            AverageBet = mean(Bet),
            TotalProfit = sum(Profit),
            TotalLosses = sum(Losses), 
            GamesWon = sum(GameWon),
            GamesLost = sum(GameLost))
head(df_clus)


#Mean-sd standardization function
mean_sd_standard <- function(x) {
  (x-mean(x))/sd(x)
}

# Apply the function to each numeric variable in the clustering set
df_standardized=df_clus %>%
  mutate_if(is.numeric, mean_sd_standard)


summary(df_standardized)

set.seed(20190101)

df_standardized[is.na(df_standardized)]=0
any(is.na(df_standardized))

#Removing the non-numeric data before training the model
df_standardized$Username=NULL

#Trainig the model

# Clustering the players using kmeans with five clusters
cluster_solution <- kmeans(df_standardized, centers =5)


#Cluster assignments back into the clustering data frame object
df_clus$cluster <- factor(cluster_solution$cluster) 

table(df_clus$cluster)


# Group by the cluster assignment and calculate averages
df_clus_avg <- df_clus %>%
  group_by(cluster) %>%
  summarize_if(funs(is.numeric),mean)

df_clus_avg


#min-max scaling function
min_max_standard <- function(x) {
  (x-min(x))/(max(x)-min(x))
}


# Apply this function to each numeric variable in the bustabit_clus_avg object
df_avg_minmax <- df_clus_avg %>%
  mutate_if(is.numeric, min_max_standard)

install.packages('GGally')
library(GGally)

# Create a parallel coordinate plot of the values
ggparcoord(df_avg_minmax, columns = c(2,3,4,5,6,7), 
           groupColumn = 'cluster', scale = "globalminmax", order = "skewness")


# Calculate the principal components of the standardized data
my_pc <- as.data.frame(prcomp(df_standardized)$x)


# Store the cluster assignments in the new data frame
my_pc$cluster <- df_clus$cluster

ggplot(my_pc,aes(x=PC1,y=PC2,color=cluster))+geom_point()

# Assign cluster names to clusters 1 through 5 in order
cluster_names <- c(
  "Risky Commoners",
  "High Rollers",
  "Cautious Commoners",
  "Risk Takers",
  "Strategic Addicts"
)

# Append the cluster names to the cluster means table
df_clus_avg_named <- df_clus_avg %>%
  cbind(Name = cluster_names)

# View the cluster means table with your appended cluster names
df_clus_avg_named
