
#Load Libraries 
# Standard - Transform and data visualization 
library(tidyverse)
library(dplyr)
library(tidyquant)

# Modeling
library(parsnip)

# Pre-processing & Sampling
library(recipes)
library(rsample)
library(workflows)

# Modeling Error Metrics
library(yardstick)

# Plotting Decision Trees
library(rpart)
library(rpart.plot)

# Read Data
bike_orderlines_tbl <- read_rds("data/bike_orderlines.rds")
glimpse(bike_orderlines_tbl)

bikes_tbl <- readRDS('data/bike_features_tbl.rds')
glimpse(bikes_tbl)

# Step 1: Data preparation
# Data Exploration

model_sales_tbl <- bike_orderlines_tbl %>%
  select(total_price, model, category_2, frame_material) %>%
  
  group_by(model, category_2, frame_material) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  arrange(desc(total_sales))

model_sales_tbl %>%
  mutate(category_2 = as_factor(category_2) %>% 
           fct_reorder(total_sales, .fun = max) %>% 
           fct_rev()) %>%
  
  ggplot(aes(frame_material, total_sales)) +
  geom_violin() +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#3e502c") +
  #coord_flip() +
  facet_wrap(~ category_2) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
  theme_tq() +
  labs(
    title = "Total Sales for Each Model",
    x = "Frame Material", y = "Revenue"
  )

#Function separate_bike_model  

separate_bike_model <- function(data, keep_model_column = TRUE, append = TRUE) {
  
  # Append
  if (!append) {
    data <- data %>% select(model)
  }
  
  # Pipeline
  output_tbl <- data %>%
    
    # select(model) %>%
    
    # Fix typo
    mutate(model = case_when(
      model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
      model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
      model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
      TRUE ~ model
    )) %>%
    
    # separate using spaces
    separate(col     = model, 
             into    = str_c("model_", 1:7), 
             sep     = " ", 
             remove  = FALSE, 
             fill    = "right") %>%
    
    # creating a "base" feature
    mutate(model_base = case_when(
      
      # Fix Supersix Evo
      str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
      
      # Fix Fat CAAD bikes
      str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
      
      # Fix Beast of the East
      str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
      
      # Fix Bad Habit
      str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
      
      # Fix Scalpel 29
      str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
      
      # catch all
      TRUE ~ model_1)
    ) %>%
    
    # Get "tier" feature
    mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
    
    # Remove unnecessary columns
    select(-matches("model_[0-9]")) %>%
    
    

  return(output_tbl)
  
}


# Step 2: Spiting Training and Test Sets

bike_features_tbl <- bike_orderlines_tbl %>%
  
  select(price, model, category_2, frame_material) %>%
  distinct() %>%
  mutate(id = row_number()) %>%
  select(id, everything()) %>%
  separate_bike_model(keep_model_column = T, append = T)

bike_features_tbl


set.seed(seed = 1113)
split_obj <- rsample::initial_split(bike_features_tbl, prop = 0.80, strata = "model_base")

bike_features_tbl %>% distinct(model_base)

split_obj %>% training() %>% distinct(model_base)

split_obj %>% testing() %>% distinct(model_base)


train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)

# Step 3: Build a Model - I chose Tree-based Methods 
# 3.1 Linear Regression 
# Model 

calc_metrics <- function(model, new_data = test_tbl) {
  
  model %>%
    predict(new_data = new_data) %>%
    
    bind_cols(new_data %>% select(price)) %>%
    yardstick::metrics(truth = price, estimate = .pred)
  
}

train_tbl

model_02_linear_lm_complex <- linear_reg("regression") %>%
  set_engine("lm") %>%
  fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_02_linear_lm_complex %>% calc_metrics(new_data = test_tbl)


# 3.2.2 Feature importance ----
model_02_linear_lm_complex$fit %>%
  broom::tidy() %>%
  arrange(p.value) %>%
  mutate(term = as_factor(term) %>% fct_rev()) %>%
  
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                            size = 3) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "Linear Regression: Feature Importance",
       subtitle = "Model 02: Complex lm Model")


# Step 4: Pre-processing - Using Recipes
# Create features with the recipes package
?recipe
?step_dummy
?prep
?bake

recipe_obj <- recipe(price ~ ., data = train_tbl) %>%
  step_rm(id, model, model_tier) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  step_log(price) %>%
  step_center(price) %>%
  step_scale(price) %>%
  prep()

bake(recipe_obj, train_tbl) %>% glimpse()

train_transformed_tbl <- bake(recipe_obj, train_tbl)
test_transformed_tbl  <- bake(recipe_obj, test_tbl)

tidy(recipe_obj)
scale <- tidy(recipe_obj, 5)
center <- tidy(recipe_obj, 4)

# Step 5: Bundle the model and recipe with the workflow package

linear_spec <- model_02_linear_lm_complex$spec

bike_workflow <- workflow() %>% 
  add_model(linear_spec) %>% 
  add_recipe(recipe_obj)

bike_workflow

# Step  6: Evaluate your model with the yardstick package

bike_fit <- 
  bike_workflow %>% 
  fit(data = train_tbl)

bike_fit %>% 
  pull_workflow_fit() %>% 
  tidy()

predict(bike_fit, test_tbl)

bike_pred <- 
  predict(bike_fit, test_tbl, type = "prob") %>% 
  bind_cols(test_tbl %>% select(total_price, model, category_2, frame_material)) 

#Now I should calculate ROC but there is a problem
# I can't fix it. Sorry! 





