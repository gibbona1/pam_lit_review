library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)
library(bib2df)

folder <- "C:/Users/Anthony"
cnames <- c("Year", "Title", "Authors", "Journal", "Abstract", "Keywords", "Language", "DOI", "URL", "Type")

#Scopus
df1 <- read.csv(file.path(folder, "Downloads", "scopus.csv")) 
df1_sub <- df1 %>%
  select(Year, Title, Authors, Source.title, Abstract, Index.Keywords, Language.of.Original.Document, DOI, Link, Document.Type) %>%
  rename_with(~ cnames) %>%
  mutate(Source = "Scopus")

#Google Scholar (using Publish or Perish)
df2 <- read.csv(file.path(folder, "Downloads", "PoPCites.csv"))
df2_sub <- df2 %>%
  mutate(Keywords = "",
         Language = "English",
         URL = ArticleURL,
         Journal = Source,
         Source = "Google Scholar") %>%
  select(Year, Title, Authors, Journal, Abstract, Keywords, Language, DOI, URL, Type, Source)

#Web of Science
df3 <- read_excel(file.path(folder, "Downloads", "savedrecs.xls"))
df3_sub <- df3 %>% 
  select(`Publication Year`, `Article Title`, Authors, `Source Title`, Abstract, `Author Keywords`, Language, DOI, `DOI Link`, `Document Type`) %>%
  rename_with(~ cnames) %>%
  mutate(Source = "Web of Science")


df_all <- rbind(df1_sub, df2_sub, df3_sub) %>%
  filter(Year >= 2000)

ggplot(df_all, aes(x=Year)) +
  geom_histogram(stat = "count")


df_sub <- df_all %>%
  mutate(lower_title = trimws(tolower(Title)) %>% str_extract("^[^.]+") %>% str_trim(),
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
  filter((DOI != "") | (URL != "")) 

df_sub %>%
  nrow()

df_sub <- df_sub %>% 
  filter(!str_detect(lower_title, paste(paste(c("a", "systematic", "comprehensive", "literature"), "review"), collapse = "|"))) %>%
  filter(!str_detect(lower_title, "survey|perspective")) %>%
  filter(!str_detect(lower_abs, "review|survey|perspective"))

df_sub %>%
  nrow()

# Perform the left join
df_sub <- df_sub %>%
  filter(!(Type %in% c("Review", "Note", "Conference review", "Letter", "Editorial")))

df_sub %>%
  nrow() 

df_sub <- df_sub %>%
  left_join(df2[, c("Title", "ArticleURL", "CitesURL")], by = "Title") 
  
df_sub %>% 
  nrow()

df_sub %>%
  filter(is.na(Type) | Type != "CITATION") %>%
  filter(!(Source == "Google Scholar" &
    (is.na(ArticleURL) | ArticleURL == "") &
    (is.na(CitesURL) | CitesURL == "") &
    (is.na(DOI) | DOI == ""))
  ) %>%
  select(-Type, -ArticleURL, -CitesURL) %>% 
  count(Source)

df_sub %>%
  nrow()

df_sub <- df_sub %>%
  left_join(df3[, "Article Title"], by = join_by(Title == `Article Title`)) %>%
  filter(!str_detect(Type, "Review") | is.na(Type))

df_sub %>% 
  nrow()

incl_terms <- c("turbine", "wind", "acoustics", "bio", "monitor", "bioacoust", "ecoacoust", "vocali", "biodiversity", "animal sound", "soundscape", "animal", "bat", "bird", "cetacean", "insect", "mammal", "wind farm", "signal processing", "acoustic indices", "acoustic index", "machine learning")

df_sub <- df_sub %>%
  filter(str_detect(lower_title, paste(incl_terms, collapse = "|"))) %>%
  filter(str_detect(lower_abs, paste(incl_terms, collapse = "|"))) 

df_sub %>%
  nrow()

excl_terms <- c("soil chemistry", "energy consumption", "turbine pitch", "soil chemistry", "energy consumption", 
                "underwater", "seafloor",
                "compounds", "organic", "effluent", "sediment", "toxic", "contaminant", "pollution", "saliniz", "genetic erosion", "pesticide", "insecticide", "chlorine",
                "grounding", "maintenance support", "wind-storage", "wind storage", "energy storage", "battery", "load forecasting", "synchronous oscillation", "construction", "spatial planning",
                "wind power curve", "frequency control", "wind speed assessment", "generator control",
                "commentary", "politics", "wave power", "solar power", "engine construction",
                "room shape", "landscape connectivity", "hearing aid", "chemical"
                )

df_sub <- df_sub %>%
  mutate(lower_key = tolower(Keywords)) %>%
  filter(!str_detect(lower_abs, paste(excl_terms, collapse = "|"))) %>%
  filter(!str_detect(lower_title, paste(excl_terms, collapse = "|"))) %>%
  filter(!str_detect(lower_key, paste(excl_terms, collapse = "|")))

df_sub %>%
  nrow()

workshop_excl <- c("workshop", "conference", "symposium", "seminar", "workshop", "meeting", "congress", "forum", "exhibition", "event", "webinar", "seminar", "summit", "convention", "roundtable", "panel")
df_sub2 <- df_sub %>% 
  distinct(lower_title, .keep_all = TRUE) %>%
  filter(!str_detect(lower_title, paste(workshop_excl, collapse = "|"))) %>%
  filter(!str_detect(lower_abs, paste(workshop_excl, collapse = "|")))

df_sub2 %>%
  nrow()

df_sub2 %>%
  count(Source)

ggplot(df_sub2, aes(x=Year)) +
  geom_histogram(stat = "count")

#write.csv(df_sub2, file.path(folder, "Downloads", "filtered_literature.csv"))
#write.csv(df_sub2, file.path(folder, "Downloads", "filtered_literature.csv"))

library(bib2df)

# Convert df_sub2 to a BibTeX dataframe
bib_df <- df_sub2 %>%
  mutate(
    Type = str_to_lower(Type),
    bibtype = case_when(
      Type == "Book chapter" ~ "incollection",
      Type == "Article" ~ "article",
      Type == "Conference paper" ~ "inproceedings",
      Type == "" ~ "misc",
      Type == "HTML" ~ "misc",
      Type == "PDF" ~ "misc",
      Type == "BOOK" ~ "book",
      Type == "CITATION" ~ "misc",
      Type == "Article; Early Access" ~ "article",
      Type == "Review" ~ "article",
      Type == "Proceedings Paper" ~ "inproceedings",
      Type == "Article; Data Paper" ~ "article",
      Type == "Article; Book Chapter" ~ "incollection",
      TRUE ~ "misc" # Default for any other unrecognized types
    ),
    bibkey = paste0("ref", row_number())
  ) %>%
  select(category=bibtype, bibtexkey=bibkey, Title, Author = Authors, Year, Journal = Journal, Abstract, Keywords, DOI, URL)

# Write the BibTeX dataframe to a .bib file
df2bib(bib_df, file = "filtered_literature.bib")
