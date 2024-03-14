# Ayesha Tariq
# capstone continuation
# week 3

install.packages("tidyr")
install.packages('spotifyr')

library(ggplot2)
library(tidyverse)
library(knitr)
library(spotifyr)
library(dplyr)
library(tidyr)
library(sjPlot)

# download data
data_og <- read.csv("Desktop/capstone_data.csv")
spotify_songs1 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')


# data cleaning

data_og[data_og == ""] <- NA
kaggle_data <- data_og[complete.cases(data_og), ]

# converting categorical values to characters

spotify_songs <- spotify_songs1 %>%
  mutate(key = as.character(key))

# remove unecessary columns
spotify_songs <- spotify_songs1 %>%
  select(-track_id, -track_name, -track_popularity -track_artist, -track_album_id, -track_album_name, -track_album_release_date, -playlist_subgenre, -playlist_name, -playlist_id)


#look at each genre subgroup
edm <- spotify_songs[spotify_songs$playlist_genre == "edm",]
rap <- spotify_songs[spotify_songs$playlist_genre == "rap",]
pop <- spotify_songs[spotify_songs$playlist_genre == "pop",]
rb <- spotify_songs[spotify_songs$playlist_genre == "r&b",]
latin <- spotify_songs[spotify_songs$playlist_genre == "latin",]
rock <- spotify_songs[spotify_songs$playlist_genre == "rock",]

# create something to get the mode 
calculate_mode <- function(x) {
  tab <- table(x)
  mode_values <- names(tab)[tab == max(tab)]
  
  if (all(mode_values %in% c("0", "1"))) {
    return(paste(mode_values, collapse = ", "))
  } else if (length(mode_values) == 1) {
    return(mode_values)
  } else {
    return("Multiple modes")
  }
}


# Create a new dataset with aggregated values
genres <- spotify_songs %>%
  group_by(playlist_genre) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(is.character), calculate_mode))

# renaming mode to "majorness"
genres <- genres %>%
  rename(majorness = mode)

# renaming genre to Fav.genre
genres1 <- genres %>%
  rename(Fav.genre = playlist_genre)

#making sure the genre names are uniform
genres1 <- genres1 %>%
  rownames_to_column(var = "new_row_names_column") %>%
  select(-column_of_interest) %>%
  column_to_rownames(var = "new_row_names_column")

genres1 <- genres %>%
  rename()
genres1 <- genres %>%
  rename(Fav.genre = playlist_genre)
genres1 <- genres %>%
  rename(Fav.genre = playlist_genre)
genres1 <- genres %>%
  rename(Fav.genre = playlist_genre)
genres1 <- genres %>%
  rename(Fav.genre = playlist_genre)
genres1 <- genres %>%
  rename(Fav.genre = playlist_genre)


## creating a new dataset with only fav genres that are in spotify data
updated_kaggle <- kaggle_data %>%
  filter(Fav.genre %in% c("EDM", "Latin", "Pop", "R&B", "Rap", "Rock"))


# rename
genres1[1, "Fav.genre"] <- "EDM"
genres1[2, "Fav.genre"] <- "Latin"
genres1[3, "Fav.genre"] <- "Pop"
genres1[4, "Fav.genre"] <- "R&B"
genres1[5, "Fav.genre"] <- "Rap"
genres1[6, "Fav.genre"] <- "Rock"

# combine the two groups 

fav_merged <- left_join(updated_kaggle, genres1, by = "Fav.genre")

### MODELING

# ANXIETY

# first order, most kaggle predictor predictors
model_1a <- lm(Anxiety ~ Age + Hours.per.day + Instrumentalist + Composer, data = fav_merged)
summary(model_1a)
BIC(model_1a)
tab_model(model_1a)

# first order, fewer kaggle predictor predictors
model_2a <- lm(Anxiety ~ Age + Hours.per.day, data = fav_merged)
summary(model_2a)
BIC(model_2a)
tab_model(model_2a)

# first order, with most kaggle and some spotify predictors
model_3a <- lm(Anxiety ~ Age + Hours.per.day + Instrumentalist + Composer + majorness + energy + danceability, data = fav_merged)
summary(model_3a)
BIC(model_3a)
tab_model(model_3a)

# first order, with some kaggle and some spotify predictors: results in slightly better R^2, adjusted R^2, and BIC values
model_4a <- lm(Anxiety ~ Age + Hours.per.day + majorness + energy, data = fav_merged)
summary(model_4a)
BIC(model_4a)
tab_model(model_4a)

# with some kaggle and fewer spotify predictors: results in slightly better R^2, adjusted R^2, and BIC values
model_5a <- lm(Anxiety ~ Age + Hours.per.day + majorness  , data = fav_merged)
summary(model_5a)
BIC(model_5a)
tab_model(model_5a)


# interaction model with some kaggle and some spotify predictors
model_6a <- lm(Anxiety ~ Age*Hours.per.day*majorness*energy , data = fav_merged)
summary(model_6a)
BIC(model_6a)
tab_model(model_5a)

# interaction with just majorness 
model_7a <- lm(Anxiety ~ Age*Hours.per.day*majorness , data = fav_merged)
summary(model_7a)
BIC(model_7a)
tab_model(model_5a)


# DATA REPRESENTATION
# predicted values from the best linear regression model
model_5ab <- predict(model_5a)

# data frame creation
plot_data <- data.frame(Actual = fav_merged$Anxiety, Predicted = model_5ab)

# scatter plot of observed vs. predicted values with a regression line
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Observed vs. Predicted Values", 
       x = "Actual", y = "Predicted") + 
  theme_minimal()







# DEPRESSION


# first order, most kaggle predictor predictors
model_1d <- lm(Depression ~ Age + Hours.per.day + Instrumentalist + Composer, data = fav_merged)
summary(model_1d)
BIC(model_1d)
tab_model(model_1a)

# first order, fewer kaggle predictor predictors
model_2d <- lm(Depression ~ Age + Hours.per.day, data = fav_merged)
summary(model_2d)
BIC(model_2d)
tab_model(model_2a)

# first order, with most kaggle and some spotify predictors
model_3d <- lm(Depression ~ Age + Hours.per.day + Instrumentalist + Composer + majorness + energy + danceability, data = fav_merged)
summary(model_3d)
BIC(model_3d)
tab_model(model_3a)

# first order, with some kaggle and some spotify predictors: results in slightly better R^2, adjusted R^2, and BIC values
model_4d <- lm(Depression ~ Age + Hours.per.day + majorness + energy, data = fav_merged)
summary(model_4d)
BIC(model_4d)
tab_model(model_4a)

# with some kaggle and fewer spotify predictors: results in slightly better R^2, adjusted R^2, and BIC values
model_5d <- lm(Depression ~ Age + Hours.per.day + majorness  , data = fav_merged)
summary(model_5d)
BIC(model_5d)
tab_model(model_5a)


# interaction model with some kaggle and some spotify predictors
model_6d <- lm(Depression ~ Age*Hours.per.day*majorness*energy , data = fav_merged)
summary(model_6d)
BIC(model_6d)
tab_model(model_5a)

# interaction with just majorness 
model_7d <- lm(Depression ~ Age*Hours.per.day*majorness , data = fav_merged)
summary(model_7d)
BIC(model_7d)
tab_model(model_5a)



### representing the data

# table for all the R^2, adjusted R^2, and BIC values
models <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model ")
r_squared <- c(0.06816, 0.06954, 0.09349, 0.09628, 0.09348, 0.1246, 0.1099)
adjusted_r_squared <- c(0.06253, 0.05822, 0.08247, 0.07688, 0.08524, 0.08331, 0.09079)
bic <- c(1639.012, 1650.142, 1641.43, 1657.834, 1635.623, 1693.688, 1652.761)

# Create a data frame
model_data <- data.frame(Model = models, R_squared = r_squared, Adjusted_R_squared = adjusted_r_squared, BIC = bic)

# Print the table
print(model_data)




# Predicted values from the linear regression model
predicted <- predict(model1)


model_1ab <- predict(model_1a)

# Create a data frame containing the observed and predicted values
plot_data <- data.frame(Actual = fav_merged$Anxiety, Predicted = model_1ab)

# Create a scatter plot of observed vs. predicted values with a regression line
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +  # Add points for each observation
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(title = "Observed vs. Predicted Values", 
       x = "Actual", y = "Predicted") +  # Add titles for axes
  theme_minimal()

# second anxiety model 

# Predicted values from the linear regression model
model_2ab <- predict(model_2a)

# Create a data frame containing the observed and predicted values
plot_data1 <- data.frame(Actual = fav_merged$Anxiety, Predicted = model_2ab)

# Create a scatter plot of observed vs. predicted values with a regression line
ggplot(plot_data1, aes(x = Actual, y = Predicted)) +
  geom_point() +  # Add points for each observation
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(title = "Observed vs. Predicted Values", 
       x = "Actual", y = "Predicted") +  # Add titles for axes
  theme_minimal()


### INTERACTION MODELS

# anxiety, with majorness
model_ia <- lm(Anxiety ~ Age*majorness + Hours.per.day*majorness + Instrumentalist*majorness + Composer*
                 majorness, data = fav_merged)
summary(model_ia)
BIC(model_ia)
tab_model(model_ia)

# second model just majorness
model_ia2 <- lm(Anxiety ~ Age*majorness + Hours.per.day*majorness, data = fav_merged)
summary(model_ia2)
BIC(model_ia2)
tab_model(model_ia2)

# multiple interactions
model_ia <- lm(Anxiety ~ Age*majorness + Hours.per.day*majorness + Instrumentalist*majorness + Composer*
                 majorness, data = fav_merged)
summary(model_ia)
BIC(model_ia)
tab_model(model_ia)


# multiple interactions, cont
model_ia2 <- lm(Anxiety ~ Age*Hours.per.day + Instrumentalist*Composer + majorness*danceability + majorness*
                 energy, data = fav_merged)
summary(model_ia2)
BIC(model_ia2)
tab_model(model_ia2)

model_ia3 <- lm(Anxiety ~ Age*Hours.per.day + Instrumentalist*Composer + majorness*danceability + majorness*
                  energy, data = fav_merged)
summary(model_ia3)
BIC(model_ia3)
tab_model(model_ia3)

# 

model3 <- lm(Anxiety ~ Age + Hours.per.day + Instrumentalist + Composer + danceability + energy + majorness + danceability*energy + danceability*majorness + energy*majorness, data = fav_merged)
summary(model3)

# 
model_5 <- lm(Anxiety ~ Age + Hours.per.day +  majorness + danceability, data = fav_merged)
summary(model_5)
BIC(model_5)
tab_model(model_5)

# find the correlation between the outcome variables

correlation_matrix <- cor(data[, c(28, 29, 30, 31)])


install.packages("gplots")
library(gplots)

heatmap.2(correlation_matrix, 
          trace = "none", 
          col = colorRampPalette(c("blue", "white", "red"))(20))

# linear regression

# looking at just majorness
model_a <- lm(Anxiety ~ majorness, data = fav_merged)
summary(model_a)

model_d <- lm(Depression ~ majorness, data = fav_merged)
summary(model_d)

model_o <- lm(OCD ~ majorness, data = fav_merged)
summary(model_o)

model_i <- lm(Insomnia ~ majorness, data = fav_merged)
summary(model_i)

# multiple regression
model <- lm(Depression ~ danceability + energy + majorness + 
              acousticness + instrumentalness + liveness + tempo + valence,
            data = fav_merged)

# Display the summary of the model
summary(model)


model2 <- lm(Depression ~ danceability + energy + majorness + 
              acousticness + instrumentalness + liveness + tempo + valence + Age + Hours.per.day + Instrumentalist + Composer,
            data = fav_merged)
summary(model2)

# Standardized coefficients for Depression model
summary(model_d)$coefficients[, "Estimate"] / sd(fav_merged$Depression)


####

# first order model - looking at just Kaggle data

# all of my kaggle variables, for anxiety
model_1 <- lm(Anxiety ~ Age + Hours.per.day + Instrumentalist, data = fav_merged)
summary(model_1)
BIC(model_1)

model_2 <- lm(Anxiety ~ Age, data = fav_merged)
summary(model_2)
BIC(model_2)

model_3 <- lm(Anxiety ~ Age + Hours.per.day, data = fav_merged)
summary(model_3)
BIC(model_3)

# model_3 <- lm(Anxiety ~ Composer, data = fav_merged)
#summary(model_3)
#BIC(model_2)
 # composer is not statistically significant? therefore do we need to include it in multiple regression models?


### interaction models

# adding majorness
model_4 <- lm(Anxiety ~ Age*majorness, data = fav_merged)
summary(model_4)
BIC(model_4)

model_5 <- lm(Anxiety ~ Age*energy, data = fav_merged)
summary(model_5)
BIC(model_5)

model_6 <- lm(Anxiety ~ Age*danceability, data = fav_merged)
summary(model_6)
BIC(model_6)






