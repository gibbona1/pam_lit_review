library(dplyr)
folder <- "C:/Users/Anthony"
cnames <- c("Year", "Title", "Abstract", "Keywords", "Language", "DOI")

#Scopus
df1 <- read.csv(file.path(folder, "Downloads", "scopus.csv"))
colnames(df1)
df1[1,]
df1$Abstract[1]
df1_sub <- df1[, c("Year", "Title", "Abstract", "Index.Keywords", "Language.of.Original.Document", "DOI")]
colnames(df1_sub) <- cnames
df1_sub$Source <- "Scopus"

#Google Scholar (using Publish or Perish)
df2 <- read.csv(file.path(folder, "Downloads", "PoPCites.csv"))
colnames(df2)
df2[1,]
df2$Abstract[1]
df2_sub <- df2 %>%
  select(Year, Title, Abstract) %>%
  mutate(Keywords = "",
         Language = "English",
         DOI = df2$DOI,
         Source = "Google Scholar")


library(readxl)
#Web of Science
df3 <- read_excel(file.path(folder, "Downloads", "savedrecs.xls"))
colnames(df3)
df3[1,]
df3$Abstract[1]
df3_sub <- df3[,c("Publication Year", "Article Title", "Abstract", "Author Keywords", "Language", "DOI")]
colnames(df3_sub) <- cnames
df3_sub$Source <- "Web of Science"


df_all <- rbind(df1_sub, df2_sub, df3_sub)

library(ggplot2)
library(dplyr)
library(stringr)

ggplot(df_all, aes(x=Year)) +
  geom_histogram(stat = "count")


df_sub <- df_all %>%
  mutate(lower_title = trimws(tolower(Title)),
         lower_abs = trimws(tolower(Abstract))) %>% 
  distinct(lower_title, .keep_all = TRUE)

df_sub %>%
  nrow()
  
df_sub <- df_sub %>%
  filter(Abstract != "") %>%
  filter(Abstract != "[No abstract available]") 

df_sub %>%
  nrow()

df_sub <- df_sub %>%
  filter(!is.na(Year)) %>%
  filter(Language == "English") 

df_sub %>%
  nrow()

df_sub <- df_sub %>%
  filter(DOI != "") 

df_sub %>%
  nrow()

df_sub <- df_sub %>%
  filter(!stringr::str_detect(Abstract, "review"))

df_sub %>%
  nrow()

# Perform the left join
df_sub <- df_sub %>%
  left_join(df1[, c("Title", "Document.Type")], by = "Title")

df_sub %>%
  nrow()

# Filter the results
df_sub <- df_sub %>%
  filter(!(Document.Type %in% c("Review", "Note", "Conference review", "Letter", "Editorial")))

df_sub %>%
  nrow()

df_sub <- df_sub %>%
  left_join(df3[, c("Article Title", "Document Type")], by = join_by(Title == `Article Title`))

df_sub %>%
  nrow()

df_sub <- df_sub %>%
  filter(!str_detect(`Document Type`, "Review") | is.na(`Document Type`)) 

df_sub %>% 
  nrow()

incl_terms <- c("turbine", "wind", "acoustics", "bio", "monitor", "passive acoustic monitoring", "species monitoring", "bioacoust", "ecoacoust", "vocali", "biodiversity", "animal sound", "soundscape", "species monitoring", "animal", "bat", "bird", "cetacean", "insect", "mammal", "wind farm", "environmental impact assessment", "signal processing", "acoustic indices", "acoustic index", "machine learning")

df_sub <- df_sub %>%
  filter(str_detect(lower_title, paste(incl_terms, collapse = "|"))) %>%
  filter(str_detect(lower_abs, paste(incl_terms, collapse = "|"))) 

df_sub %>%
  nrow()

excl_terms <- c("underwater", "seafloor", "soil chemistry", "energy consumption", "images", "turbine pitch", 
                "compounds", "organic", "effluent", "sediment", "toxic", "contaminant", "pollution", "saliniz", "genetic erosion", "pesticide", "insecticide", "chlorine",
                "grounding", "maintenance support", "wind-storage", "wind storage", "energy storage", "battery", "load forecasting", "synchronous oscillation", "construction", "spatial planning",
                "wind power curve", "frequency control", "wind speed assessment", "generator control",
                "commentary", "politics", "wave power", "solar power", "engine construction"
                )

df_sub2 <- df_sub %>%
  mutate(lower_key = tolower(Keywords)) %>%
  filter(!str_detect(lower_abs, paste(excl_terms, collapse = "|"))) %>%
  filter(!str_detect(lower_title, paste(excl_terms, collapse = "|"))) %>%
  filter(!str_detect(lower_key, paste(excl_terms, collapse = "|"))) 


df_sub2 %>%
  #nrow()
  #filter(Year > 2007) %>%
  count(Source)


# Load necessary libraries
library(tidyverse)
library(text2vec)
library(tm)
library(cosine)
library(SnowballC)

# Expand search string
expand_search_string <- function(search_string) {
  terms <- strsplit(search_string, "\\|")[[1]]
  expanded_string <- paste(terms, collapse = " ")
  expanded_string
}

# Generate word embeddings
generate_embeddings <- function(text, model) {
  tokens <- word_tokenizer(text)
  it <- itoken(tokens, progressbar = FALSE)
  vectorizer <- vocab_vectorizer(vocab = create_vocabulary(it))
  dtm <- create_dtm(it, vectorizer)
  embedding <- model$transform(dtm)
  embedding
}

# Calculate cosine similarity
calculate_similarity <- function(vector1, vector2) {
  cosine(vector1, vector2)
}

# Load a pre-trained model or train a simple one
train_embedding_model <- function(text_data) {
  tokens <- word_tokenizer(text_data)
  it <- itoken(tokens, progressbar = FALSE)
  model <- GloVe$new(rank = 100, x_max = 10)
  model$fit_transform(create_dtm(it, vectorizer = vocab_vectorizer(create_vocabulary(it))))
  model
}

# Step 1: Expand the search string
search_string <- paste(incl_terms, collapse = "|")
expanded_search_string <- expand_search_string(search_string)

# Step 2: Train or load word embeddings model
text_data <- c(expanded_search_string, df_sub$Abstract)
model <- train_embedding_model(text_data)

# Step 3: Generate embeddings for the search string and abstracts
search_embedding <- generate_embeddings(expanded_search_string, model)
abstract_embeddings <- lapply(df_sub$Abstract, generate_embeddings, model = model)

# Step 4: Calculate similarity scores
similarity_scores <- sapply(abstract_embeddings, calculate_similarity, vector2 = search_embedding)

# Combine results into a dataframe
result_df <- df_sub %>%
  mutate(Similarity = similarity_scores)

# View results
print(result_df)



ggplot(df_sub2, aes(x=Year)) +
  geom_histogram(stat = "count")

#write.csv(df_sub, file.path(folder, "Downloads", "tmp_df_sub.csv"))
