#api.R
#API for final project: predicts term deposit subscription with best model from vignette i.e, random forest with mtry=2

library(plumber)
library(tidyverse)
library(tidymodels)

bank_raw <- read_delim("data/train.csv", delim = ";")
bank_all <- bank_raw |>
  mutate(
    across(where(is.character), as.factor)
  )

bank <- bank_all |>
  select(y, housing, poutcome, age, balance, campaign)

bank_recipe <- recipe(y ~ ., data = bank)

rf_spec <- rand_forest(mtry = 2) |>
  set_engine("ranger") |>
  set_mode("classification")


rf_workflow <- workflow() |>
  add_recipe(bank_recipe) |>
  add_model(rf_spec)

set.seed(558)
final_model <- rf_workflow |>
  fit(bank)

most_prevalent <- function(x) {
  counts <- table(x) # count at each level 
  max_count <- max(counts)
  names(counts)[counts == max_count][1]
}

# default values: mean for numeric predictors, most prevalent level for categorical 
default_age <- mean(bank$age)
default_balance <- mean(bank$balance)
default_campaign <- mean(bank$campaign)
default_housing <- most_prevalent(bank$housing)
default_poutcome <- most_prevalent(bank$poutcome)

#* Predict whether a client subscribes to a term deposit 
#* @param age client age in years 
#* @param balance Average yearly balance in euros
#* @param campaign Number of contacts during this campaign 
#* @param housing Has a housing loan? (yes or no)
#* @param poutcome Outcome of the previous campaign (failure, other, success or unknown)
#* @get /pred
function(age = default_age, balance = default_balance, campaign = default_campaign,
         housing = default_housing, poutcome = default_poutcome) {
  
  if(!(housing %in% levels(bank$housing))) {
    return(list(error="housing must be yes or no"))
  }
  
  if(!(poutcome %in% levels(bank$poutcome))) {
    return(list(error="poutcome must be failure, other, success or unknown"))
  }
  
  if(is.na(as.numeric(age)) | is.na(as.numeric(balance)) | is.na(as.numeric(campaign))) {
    return(list(error="age, balance and campaign must be numbers"))
  }
  
  #convert the input strings to match the training data
  new_client <- tibble(
    age = as.numeric(age),
    balance = as.numeric(balance),
    campaign = as.numeric(campaign),
    housing = factor(housing, levels = levels(bank$housing)),
    poutcome = factor(poutcome, levels = levels(bank$poutcome))
  )
  
  list(
    predicted_class = predict(final_model, new_client)$.pred_class,
    prob_yes = predict(final_model, new_client, type = "prob")$.pred_yes
  )
}

#query with http://localhost:8000/pred
#query with http://localhost:8000/pred?housing=yes&poutcome=success
#query with  http://localhost:8000/pred?age=20&balance=250&campaign=1&housing=no&poutcome=success
#query with  http://localhost:8000/pred?age=65&balance=2200&campaign=5&housing=yes&poutcome=failure

#* Info about this API
#* @get /info
function() {
  list(
    name = "bdevapa",
    url = "https://github.com/bdevapa/st558_final_project"
  )
} 

#query with http://localhost:8000/info 

