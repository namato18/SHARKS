library(dplyr)
library(lubridate)
library(ggplot2)
library(xgboost)
library(caret)
library(stringr)

df = read.csv('dat/GSAF5.xls.csv')
df = df[,1:23]

# CLEAN DATES
formatted_dates = dmy(df$Date)
more_dates = ymd(df$Date)
formatted_dates[is.na(formatted_dates) & !is.na(more_dates)] = more_dates[is.na(formatted_dates) & !is.na(more_dates)]
more_dates = my(df$Date)
formatted_dates[is.na(formatted_dates) & !is.na(more_dates)] = more_dates[is.na(formatted_dates) & !is.na(more_dates)]

df$Date = formatted_dates
df$Month = as.factor(lubridate::month(df$Date))



# CLEAN ACTIVITY
df = df %>% filter(Type %in% c("Unprovoked", "Provoked","Boat","Boating","Watercraft"))

activities = df$Activity

activities[grepl('fish|hook|lobst|stand|net', activities, ignore.case = TRUE)] = 'Fishing'
activities[grepl('swim|wad|float|clean|jump|bath|wash', activities, ignore.case = TRUE)] = 'Swimming'
activities[grepl('boogie|body board', activities, ignore.case = TRUE)] = 'Body Boarding'
activities[grepl('surf|skim', activities, ignore.case = TRUE)] = 'Surfing'
activities[grepl('diving|pick|feed|snork|wreck|clam|div', activities, ignore.case = TRUE)] = 'Diving'
activities[grepl('paddle|sup', activities, ignore.case = TRUE)] = 'Paddle Boarding'
activities[grepl('fell|rowing|boat|fall|ship|overboard|crash|capsize|yacht', activities, ignore.case = TRUE)] = 'Fell Off Boat'
activities[grepl('canoe|raft|dinghy|kayak', activities, ignore.case = TRUE)] = 'Canoe'



df$Activity = activities

df = df %>%
  filter(df$Activity %in% c('Fishing', 'Swimming', 'Body Boarding', 'Diving', 'Surfing', 'Paddle Boarding', 'Fell Off Boat', 'Canoe'))

# CLEAN SPECIES

species = df$Species

species[grepl('white', species, ignore.case = TRUE)] = 'Great White Shark'
species[grepl('tip', species, ignore.case = TRUE)] = 'Black Tip Shark'
species[grepl('blue', species, ignore.case = TRUE)] = 'Blue Shark'
species[grepl('bull', species, ignore.case = TRUE)] = 'Bull Shark'
species[grepl('tiger', species, ignore.case = TRUE)] = 'Tiger Shark'
species[grepl('pack', species, ignore.case = TRUE)] = 'Bunch of Sharks'
species[grepl('nurse', species, ignore.case = TRUE)] = 'Nurse Shark'
species[grepl('wobb', species, ignore.case = TRUE)] = 'Wobbegong Shark'
species[grepl('mako', species, ignore.case = TRUE)] = 'Mako Shark'
species[grepl('thresh', species, ignore.case = TRUE)] = 'Thresher Shark'
species[grepl('hammer', species, ignore.case = TRUE)] = 'Hammerhead Shark'
species[grepl('ragged', species, ignore.case = TRUE)] = 'Raggedtooth Shark'
species[grepl('shovel', species, ignore.case = TRUE)] = 'Shovelnose Shark'
species[grepl('whale', species, ignore.case = TRUE)] = 'Whaler Shark'
species[grepl('lemon', species, ignore.case = TRUE)] = 'Lemon Shark'
species[grepl('sand', species, ignore.case = TRUE)] = 'Sand Tiger Shark'
species[grepl('reef', species, ignore.case = TRUE)] = 'Reef Shark'
species[grepl('galapagos', species, ignore.case = TRUE)] = 'Galapagos Shark'
species[grepl('large', species, ignore.case = TRUE)] = 'Large Shark'
species[grepl('small', species, ignore.case = TRUE)] = 'Small Shark'
species[grepl('leopard', species, ignore.case = TRUE)] = 'Leopard Shark'
species[grepl('carpet', species, ignore.case = TRUE)] = 'Carpet Shark'
species[grepl('copper', species, ignore.case = TRUE)] = 'Copper Shark'
species[grepl('dusk', species, ignore.case = TRUE)] = 'Dusky Shark'
species[grepl('spin', species, ignore.case = TRUE)] = 'Spinner Shark'

known_sharks = c('Great White Shark', "Black Tip Shark", 'Blue Shark',
                 'Bull Shark', 'Tiger Shark', 'Bunch of Sharks', 'Nurse Shark',
                 'Wobbegong Shark', 'Mako Shark', 'Thresher Shark', 'Hammerhead Shark',
                 'Raggedtooth Shark', 'Shovelnose Shark', 'Whaler Shark', 'Lemon Shark',
                 'Sand Tiger Shark', 'Reef Shark', 'Galapagos Shark', 'Large Shark',
                 'Small Shark', 'Leopard Shark', 'Carpet Shark', 'Copper Shark',
                 'Dusky Shark',"Spinner Shark")
known_sharks_index = c(1:length(known_sharks)) - 1

known_shark_df = data.frame(
  known_sharks = known_sharks,
  known_sharks_index = known_sharks_index
)

df$Species = species


df = df %>%
  filter(df$Species %in% c(known_sharks))





# REMOVE WHERE FATAL INFO IS MISSING
df = df %>% filter(Fatal..Y.N. != "",
                   Fatal..Y.N. != "M",
                   Fatal..Y.N. != "UNKNOWN",
                   Country != "",
                   !is.na(Date))
df$Fatal..Y.N.[df$Fatal..Y.N. == 'Y'] = 1
df$Fatal..Y.N.[df$Fatal..Y.N. == 'N'] = 0
df$Fatal..Y.N. = as.factor(as.numeric(df$Fatal..Y.N.))


df$Date[df$Date == "0144-07-22"] = '1944-07-22'

hour = as.numeric(str_match(string = df$Time, pattern = "(\\d+)h")[,2])
hour[hour >= 0 & hour < 12] = "Morning"
hour[hour >= 12 & hour < 17] = "Afternoon"
hour[hour >= 17 & hour < 24] = "Night"

df$Time[!is.na(hour)] = hour[!is.na(hour)]

df$Time[grepl(pattern = "morning|a.m.", x = df$Time, ignore.case = TRUE)] = "Morning"
df$Time[grepl(pattern = "noon|midday|lunch", x = df$Time, ignore.case = TRUE)] = "Afternoon"
df$Time[grepl(pattern = "night|sundown|p.m.|evening|pm|dusk", x = df$Time, ignore.case = TRUE)] = "Night"

unique(df$Time)

df$Time[!df$Time %in% c("Morning", "Afternoon", "Night")] = ""

unique(df$Time)

df = df %>% filter(Time != "")

known_sharks = unique(df$Species)
known_sharks_index = c(1:length(known_sharks)) - 1

known_shark_df = data.frame(
  known_sharks = known_sharks,
  known_sharks_index = known_sharks_index
)




# VIZ
df_countries = data.frame(table(df$Country))

df_top = df_countries %>% arrange(desc(Freq))
df_top = df_top[1:10,]

df_10 = df %>% filter(Country %in% df_top$Var1)

ggplot(data = df_10, aes(x = Country, fill = Fatal..Y.N.)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Shark Attacks")

# MODEL BUILDING

names(df)

df_ml = df %>%
  select(Month, Country, Area, Activity, Type, Fatal..Y.N., Time, Species) %>%
  filter(Area != "") %>%
  mutate(Country = toupper(Country)) %>%
  left_join(known_shark_df, join_by("Species" == "known_sharks")) %>%
  select(-Species)

country_mapping = data.frame(
  Country = unique(df_ml$Country),
  Mapping_Country = c(1:length(unique(df_ml$Country))) - 1
)

area_mapping = data.frame(
  Area = unique(df_ml$Area),
  Mapping_Area = c(1:length(unique(df_ml$Area))) - 1
)

activity_mapping = data.frame(
  Activity = unique(df_ml$Activity),
  Mapping_Activity = c(1:length(unique(df_ml$Activity))) - 1
)

type_mapping = data.frame(
  Type = unique(df_ml$Type),
  Mapping_Type = c(1:length(unique(df_ml$Type))) - 1
)

time_mapping = data.frame(
  Time = unique(df_ml$Time),
  Mapping_Time = c(1:length(unique(df_ml$Time))) - 1
)



df_ml = df_ml %>%
  left_join(area_mapping, by = "Area") %>%
  # left_join(country_mapping, by = "Country") %>%
  left_join(activity_mapping, by = 'Activity') %>%
  left_join(type_mapping, by = 'Type') %>%
  left_join(time_mapping, by = "Time") %>%
  select(-c(Area, Country, Activity, Type, Time))


# Step 1: Convert categorical variables to numeric (label encoding)
# Convert the Month column to numeric
df_ml$Month <- as.numeric(df_ml$Month)

# Convert categorical character columns to factors and then to numeric
# df_ml$Country <- as.numeric(as.factor(df_ml$Country))
# df_ml$Area <- as.numeric(as.factor(df_ml$Area))
# df_ml$Activity <- as.numeric(as.factor(df_ml$Activity))
# df_ml$Type <- as.numeric(as.factor(df_ml$Type))

# Convert the target variable 'Species' to a numeric factor
# df_ml$known_sharks_index <- as.numeric(as.factor(df_ml$known_sharks_index)) - 1  # xgboost requires labels to be numeric starting from 0

# Convert Fatal..Y.N. to numeric
df_ml$Fatal..Y.N. <- as.numeric(as.factor(df_ml$Fatal..Y.N.)) - 1  # Assuming 0/1 encoding

# Separate features and target
X <- df_ml %>% select(-known_sharks_index)
y <- df_ml$known_sharks_index

sample.split = sample(c(TRUE,FALSE), nrow(df_ml), replace = TRUE, prob=c(0.9,0.1))

x.train = as.matrix(X[sample.split,])
y.train = y[sample.split]

x.test = as.matrix(X[!sample.split,])
y.test = y[!sample.split]

# Step 2: Train the xgboost model
params <- list(
  objective = "multi:softprob",  # For multiclass classification
  num_class = length(unique(y)), # Number of classes
  eval_metric = "mlogloss"       # Evaluation metric
)

# Train the model
bst = xgboost(data = x.train,
              label = y.train,
              params = params,
              nrounds = 100,
              verbose = TRUE,
              )
# Step 3: Check feature importance
importance <- xgb.importance(model = bst)
xgb.plot.importance(importance)

dtest <- xgb.DMatrix(data = as.matrix(x.test))

pred_probs <- predict(bst, dtest)

n_predictions <- length(pred_probs) / length(unique(y))
pred_probs_matrix <- matrix(pred_probs, nrow = n_predictions, ncol = length(unique(y)), byrow = TRUE)

predicted_classes <- max.col(pred_probs_matrix) - 1

predicted_classes
y.test

length(which(predicted_classes == y.test)) / length(y.test) * 100

predictions_mapping = data.frame(
  predicted_classes = predicted_classes
) %>%
  left_join(known_shark_df, join_by("predicted_classes" == "known_sharks_index"))

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

our_own_info = data.frame(
  Month = 7,
  Fatal..Y.N. = 1,
  Mapping_Area = 5,
  # Mapping_Country = 0,
  Mapping_Activity = 2,
  Mapping_Type = 0,
  Mapping_Time = 1
)

ooi.m = as.matrix(our_own_info)

pred = predict(bst, ooi.m)

which(pred == max(pred)) - 1






