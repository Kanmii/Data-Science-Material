#09-05-2025 Week 8 - DAY 14 (Class 1)
if (!require(remotes))
  install.packages("remotes")

remotes::install_github("irudnyts/openai")
install.packages("reticulate")
install.packages("fuzzywuzzyR")

reticulate::py_config()

{
  library(openai)
  library(httr)
  library(stringr)
  library(glue)
  library(tidyverse)
  library(data.table)
  library(fuzzywuzzyR)
}

#conflicts_prefer(httr::content)

#source(apiKeys)


#Api key
api_key = "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"


african_names <- read.csv("C:/Users/fatai/OneDrive/Desktop/Tyy/DS COURSE MATERIALS/african_names.csv")

#Briefly explore your data
names(african_names)
head(african_names)
glimpse(african_names)
tail(african_names)

#Calls ChatGpt API with the given prompt and return the answer
ask_chatgpt <- function(prompt) {
  response <- POST(
    url = "https//api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      #model = "GPT-4o mini",
      messages = list(list(
        role = "user",
        content = prompt
      ))
    )
  )
  str_trim(content(response)$choices[[1]]$messages$content)
}


#Loop through the dataframe
for (i in 1:nrow(african_names)) {
  #Get the oruko for the current row
  oruko <- african_names$oruko[i]
  
  prom <- "Which country will the name {oruko} most likely come from?"
  #prom <- "Which country will the name {oruko} most likely come from?". Give just one word response, which should be name of a country"
  prom1 <- glue(prom)
  
  #detect country
  ilu <- ask_chatgpt(prom1)
  cat(ilu)
  
  #Update the predicted country column in the dataframe
  african_names$country_predicted[i] <- ilu
}

head(african_names)

#Save your result, so that you don't have to repeat the OpenAi tasks again
write.csv(african_names, "african_names_predicted_country.csv")

#Read your file again
predicted_country <- read.csv("african_names_predicted_country.csv")

# As discussed during the session, we can use fuzzymatching to have a cleaner predicted column
# but we will have to isolate the rows here.

#Use grepl to find the rows where the country_predicted column contains the string "Nigeria"
nigerian_rows <- grepl("Nigeria", predicted_country$country_predicted)

#Subset the dataframe to only the rows where the country_predicted column contains the string "Nigeria"
nigerian_names <- predicted_country[nigerian_rows, ]

#We can save the filtered country data
write.csv(nigerian_names, "nigerian_names_with_correct_names.csv")

naija_names <- read.csv("nigerian_names_with_correct_names.csv")

#With an improved prompt engineering, the work become easier.




# Using LLm for code Generation or Build App
remotes::install_github("mlverse/chattr")

#Load library
library(chattr)

#Set up your OpenAI Api Key
Sys.setenv(openapi_key <- "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")

#Pick the model to use (gpt4o or gpt35)
chattr_use("gpt4o")

#Run Chattr Shinny App as a Job (So you can still use your console)
chattr_app(as_job = TRUE)

# 1.0 loading a file


# Question
# How to open the data "/Users/drajalaagift/Documents/fdi_telco_Q2.2024.xlsx"?




library(ISLR)
data(Wage)
glimpse(Wage)

# Question
#
The Data set looks like this. Produce a single script shiny app code to visualize the Data?
#
