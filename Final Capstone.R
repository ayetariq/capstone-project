# Ayesha Tariq - Data Science Capstone

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
kaggle_data1 <- data_og[complete.cases(data_og), ]

# remove genres from kaggle data not represented in spotify data
kaggle_data <- kaggle_data1 %>%
  select(-Frequency..Classical. ,-Frequency..Country., -Frequency..Folk., -Frequency..Gospel., -Frequency..Hip.hop., -Frequency..Jazz., -Frequency..K.pop., -Frequency..Lofi., -Frequency..Metal., -Frequency..Video.game.music.)

# converting categorical values to characters

spotify_songs <- spotify_songs1 %>%
  mutate(key = as.character(key))

# remove unecessary columns
spotify_songs <- spotify_songs1 %>%
  select(-track_id, -track_name, -track_popularity, -track_artist, -track_album_id, -track_album_name, -track_album_release_date, -playlist_subgenre, -playlist_name, -playlist_id)


#look at each genre subgroup
edm <- spotify_songs[spotify_songs$playlist_genre == "edm",]
rap <- spotify_songs[spotify_songs$playlist_genre == "rap",]
pop <- spotify_songs[spotify_songs$playlist_genre == "pop",]
rb <- spotify_songs[spotify_songs$playlist_genre == "r&b",]
latin <- spotify_songs[spotify_songs$playlist_genre == "latin",]
rock <- spotify_songs[spotify_songs$playlist_genre == "rock",]

# function to calculate the mode
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

# function to convert frequency levels to numerical values
freq_to_num <- function(level) {
  numerical_values <- c("Never" = 0, "Rarely" = 0.33, "Sometimes" = 0.66, "Very frequently" = 1)
  ifelse(level %in% names(numerical_values), numerical_values[level], NA)
}

# change freq values to numeric in kaggle data
kaggle_numeric <- data.frame(
  lapply(kaggle_data, freq_to_num)
)

kaggle2 <- kaggle_data

kaggle2$Frequency..EDM. <- freq_to_num(kaggle2$Frequency..EDM.)
kaggle2$Frequency..Latin. <- freq_to_num(kaggle2$Frequency..Latin.)
kaggle2$Frequency..Pop. <- freq_to_num(kaggle2$Frequency..Pop.)
kaggle2$Frequency..R.B. <- freq_to_num(kaggle2$Frequency..R.B.)
kaggle2$Frequency..Rap. <- freq_to_num(kaggle2$Frequency..Rap.)
kaggle2$Frequency..Rock. <- freq_to_num(kaggle2$Frequency..Rock.)


# making a kaggle genre dataset
kag_sub1 <- kaggle2[,12:17]
gen_sub1 <- genres[,2:13]


# Function to perform the calculation and save each value into a new dataset
calculate_save <- function(kag_sub1, gen_sub1) {
  # Convert data frames to numeric
  kag_sub1 <- as.data.frame(lapply(kag_sub1, as.numeric))
  gen_sub1 <- as.data.frame(lapply(gen_sub1, as.numeric))

  result_list <- list()
  
  # go through each row
  for (i in 1:nrow(kag_sub1)) {
    # multiply each row of kag_sub1 by gen_sub1
    row_multiplication <- as.vector(as.matrix(kag_sub1[i,])) %*% as.matrix(gen_sub1)
    # divide by sum of each row value in kag_sub1
    row_division <- row_multiplication / sum(as.matrix(kag_sub1[i,]))
    # store values
    result_list[[i]] <- as.data.frame(row_division)
  }
  result_df <- do.call(rbind, result_list)
  
  colnames(result_df) <- colnames(gen_sub1)
  
  return(result_df)
}

final_result <- calculate_save(kag_sub1, gen_sub1)

kaggle <- kaggle2

joint_data <- cbind(kaggle, final_result)

# make a dataset where the factor joining the two is an individual's favorite genre 
fav_merged <- left_join(updated_kaggle, genres1, by = "Fav.genre")

### MODELING

# ANXIETY

# first order, most kaggle predictor predictors
model_1a <- lm(Anxiety ~ Age + Hours.per.day + Instrumentalist + Composer, data = joint_data)
summary(model_1a)
BIC(model_1a)
tab_model(model_1a)

# first order, fewer kaggle predictor predictors
model_2a <- lm(Anxiety ~ Age + Hours.per.day, data = joint_data)
summary(model_2a)
BIC(model_2a)
tab_model(model_2a)

# first order, with most kaggle and some spotify predictors
model_3a <- lm(Anxiety ~ Age + Hours.per.day + Instrumentalist + Composer + majorness + energy + danceability, data = joint_data)
summary(model_3a)
BIC(model_3a)
tab_model(model_3a)

# first order, with some kaggle and some spotify predictors: results in slightly better R^2, adjusted R^2, and BIC values
model_4a <- lm(Anxiety ~ Age + Hours.per.day + majorness + energy, data = joint_data)
summary(model_4a)
BIC(model_4a)
tab_model(model_4a)


# interaction model with some kaggle and some spotify predictors
model_6a <- lm(Anxiety ~ Age*Hours.per.day*majorness*energy , data = joint_data)
summary(model_6a)
BIC(model_6a)
tab_model(model_5a)

# interaction with just majorness 
model_7a <- lm(Anxiety ~ Age*Hours.per.day*majorness , data = joint_data)
summary(model_7a)
BIC(model_7a)
tab_model(model_5a)

# with some kaggle and fewer spotify predictors: results in slightly better R^2, adjusted R^2, and BIC values
model_5a <- lm(Anxiety ~ Age + Hours.per.day + majorness  , data = joint_data)
summary(model_5a)
BIC(model_5a)
tab_model(model_5a)

# best model with energy 
model_8a <- lm(Anxiety ~ Age + Hours.per.day + energy  , data = joint_data)
summary(model_8a)
BIC(model_8a)
tab_model(model_8a)

# best model with dance --> lowest BIC 
model_9a <- lm(Anxiety ~ Age + Hours.per.day + danceability  , data = joint_data)
summary(model_9a)
BIC(model_9a)
tab_model(model_9a)


## best model with dance with depression
model_11a <- lm(Anxiety ~ Age + Hours.per.day + danceability + Depression , data = joint_data)
summary(model_11a)
BIC(model_11a)
tab_model(model_11a)


## best model with valence with depression
model_11b <- lm(Anxiety ~ Age + Hours.per.day + valence + Depression , data = joint_data)
summary(model_11b)
BIC(model_11b)
tab_model(model_11b)

## best model with dance with OCD 
model_12a <- lm(Anxiety ~ Age + Hours.per.day + danceability + OCD , data = joint_data)
summary(model_12a)
BIC(model_12a)
tab_model(model_12a)

## best model with valence with OCD 
model_12b <- lm(Anxiety ~ Age + Hours.per.day + valence + OCD , data = joint_data)
summary(model_12b)
BIC(model_12b)
tab_model(model_12b)

## best model with dance with Insomnia 
model_13a <- lm(Anxiety ~ Age + Hours.per.day + danceability + Insomnia , data = joint_data)
summary(model_13a)
BIC(model_13a)
tab_model(model_13a)

## best model with valence with Insomnia 
model_13b <- lm(Anxiety ~ Age + Hours.per.day + valence + Insomnia , data = joint_data)
summary(model_13b)
BIC(model_13b)
tab_model(model_13b)

## best model with dance with all outcome var 
model_14a <- lm(Anxiety ~ Age + Hours.per.day + danceability + Insomnia + Depression + OCD , data = joint_data)
summary(model_14a)
BIC(model_14a)
tab_model(model_14a)

# majorness
model_15c <- lm(Anxiety ~ Age + Hours.per.day + majorness + Insomnia + Depression + OCD , data = joint_data)
summary(model_15c)
BIC(model_15c)
tab_model(model_14b)

## best model with valence with all outcome var 
model_16 <- lm(Anxiety ~ Age + Hours.per.day + danceability + Insomnia + Depression + OCD, data = clean_joint)
summary(model_16)
BIC(model_16)
tab_model(model_16)

### FINAL MODELS TO CONSIDER

## best model with dance with all outcome var 
model_14a <- lm(Anxiety ~ Age + Hours.per.day + danceability + Insomnia + Depression + OCD , data = clean_joint)
summary(model_14a)
BIC(model_14a)
tab_model(model_14a)

# best model with valence 
model_10a <- lm(Anxiety ~ Age + Hours.per.day + valence, data = clean_joint)
summary(model_10a)
BIC(model_10a)
tab_model(model_10a)

## best model with valence with all outcome var 
model_14b <- lm(Anxiety ~ Age + Hours.per.day + valence + Insomnia + Depression + OCD, data = clean_joint)
summary(model_14b)
BIC(model_14b)
tab_model(model_14b)

# mental health model
model_mental <- lm(Anxiety~Age + Insomnia + Depression + OCD, data=clean_joint)
summary(model_mental)
BIC(model_mental)
tab_model(model_mental)

# mental health with no age
model_mental_ag <- lm(Anxiety~ Insomnia + Depression + OCD, data=clean_joint)
summary(model_mental_ag)
BIC(model_mental_ag)
tab_model(model_mental_ag)

# mental health, age, hours
model_mental_agh <- lm(Anxiety~Age + Hours.per.day + Insomnia + Depression + OCD, data=clean_joint)
summary(model_mental_agh)
BIC(model_mental_agh)
tab_model(model_mental_agh, ci.force = TRUE)

## ANOVA TESTING

## compare three models: only mental health, mental health + spotify, spotify
anova_compare3 <- anova(model_10a, model_14b, model_mental)
print(anova_compare3)

anova(model_mental_ag, model_mental)

## compare 4 models: only mental health, mental health+age, mental health + spotify, spotify
anova_compare4 <- anova(model_10a, model_14b, model_mental_ag, model_mental)
print(anova_compare4)

# compare best two models
anova_compare2 <- anova(model_14b, model_mental)
print(anova_compare2)

# compare  two models
anova_compare22 <- anova(model_10a, model_mental)
print(anova_compare22)

# compare dance
anova_compare33 <- anova(model_14a, model_mental)
print(anova_compare33)

# DATA REPRESENTATION

# Compute fitted values and residuals
fitted_values <- predict(model_14b)
residuals <- residuals(model_14b)

plot(lm1$fitted.values, lm1$resid)
plot(clean_joint$fitted_values, clean_joint$residuals)


# Combine data into a data frame
plot_data <- data.frame(
  Fitted = fitted_values,
  Residuals = residuals,
  Age = clean_joint$Age,
  Hours.per.day = clean_joint$Hours.per.day,
  Depression = clean_joint$Depression,
  OCD = clean_joint$OCD,
  Insomnia = clean_joint$Insomni
)
clean_joint$Age


# Reshape data to long format for ggplot
plot_data_long <- tidyr::gather(plot_data, key = "Predictor", value = "Value", -Fitted, -Residuals)

# Plot using ggplot2
ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs. Fitted Values")


# predicted values from the best linear regression model
model_14a1 <- predict(model_14a)
model_11aa <- predict(model_11a)
model_14b1 <- predict(model_14b)

# new data frame with ommitted rows
clean_joint <- na.omit(joint_data)

## RESIDUALS VS FITTED VALUE PLOT

# Compute fitted values and residuals
fitted_values <- predict(model_mental)
residuals <- residuals(model_mental)

# Combine data into a data frame
plot_data1 <- data.frame(
  Fitted = fitted_values,
  Residuals = residuals,
  Age = clean_joint$Age,
  Hours.per.day = clean_joint$Hours.per.day,
  Depression = clean_joint$Depression,
  OCD = clean_joint$OCD,
  Insomnia = clean_joint$Insomnia
  # Add more predictor variables as needed
)
# for model 3

model_mental_agh

residuals_df <- data.frame(
  Fitted_Values = model_mental_agh$fitted.values,
  Residuals = model_mental_agh$residuals
)

# Create residuals vs. fitted values plot with ggplot
ggplot(residuals_df, aes(x = Fitted_Values, y = Residuals)) +
  geom_point(alpha=0.5) + geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(x = "Fitted Values", y = "Residuals")+ 
  theme_minimal() + geom_jitter(width = 0.25, height = 0.25,alpha = 0.5 )



## create a histogram
ggplot(clean_joint, aes(x = majorness)) +
  geom_histogram(fill = "cadetblue", color = "black", bins = 30) +
  labs(x = "Percent of Songs in Major Key", y = "Frequency in Dataset") +
  theme_minimal()

plot_data2 <- data.frame(
  Fitted = model_14b$fitted_values,
  Residuals = residuals
)

ggplot(plot_data1, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Residuals vs. Fitted Values", 
       x = "Fitted Values", y = "Residuals") + 
  theme_minimal() + geom_jitter(width = 0.25, height = 0.25,alpha = 0.5 )

ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.1, size =1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear regression line
  labs(title = "Residuals vs. Fitted Values", 
       x = "Fitted Values", y = "Residuals") + 
  theme_minimal() + geom_jitter(width = 0.2, height = 0.2)

ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Residuals vs. Fitted Values", 
       x = "Fitted Values", y = "Residuals") + 
  theme_minimal() + geom_jitter(width = 0.25, height = 0.25,alpha = 0.5 )

# other residual plot
plot(resid(model_14b)~fitted(model_14b), main = "Residual Plot for Best Model",
     ylab = "Residuals", xlab = "Fitted Values")
abline(h = 0)

# predicted values from model with no mental health as predictors
model_10a1 <- predict(model_10a)
plot_data1 <- data.frame(Actual = clean_joint$Anxiety, Predicted = model_10a1)

ggplot(plot_data1, aes(x = Actual, y = Predicted)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Observed vs. Predicted Values", 
       x = "Actual", y = "Predicted") + 
  theme_minimal()

anova_compare <- anova(model_10a, model_14b)
print(anova_compare)


#### EXPLORING THE DATA

# bar plot
ggplot(genres, aes(x = playlist_genre, y = valence)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Valence Levels Across Genres", x = "Genre", y = "Valence Level")


# DANCEABILITY

# danceability with Instrumentalist
A_v_I <- ggplot(joint_data,aes(valence, Anxiety, color=Instrumentalist)) + 
  geom_point(alpha=0.7) + geom_jitter() +
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
A_v_I

D_v_I <- ggplot(joint_data,aes(valence, Depression, color=Instrumentalist)) + 
  geom_point() + 
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
D_v_I

### saving the data

write.csv(clean_joint, "Capstone Data - Final", row.names = FALSE)
getwd()
write.csv(clean_joint, "data.csv", row.names = FALSE)

