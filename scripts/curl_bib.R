library(curl)
library(bib2df)
library(dplyr)
bib_df <- bib2df("filtered_literature.bib")

# Extract DOIs from the BibTeX dataframe
DOIlist <- bib_df$DOI
titlelist <- bib_df$TITLE
#bib_df$URL
#DOIlist <- c("10.1111/1748-5967.12330", "10.11975/j.issn.1002-6819.2017.z1.035", "10.1016/j.envsci.2019.03.017")

outFile <- "curltest.bib"
#DOIlist <- c("http://dx.doi.org/10.25607/OBP-876")
for (i in 1:length(DOIlist)) {
    if(DOIlist[i] == "") 
        next
    url <- paste0("https://doi.org/", DOIlist[i])
    print(paste0("url: ", url))
    try({
        h <- curl::new_handle() # Create a new handle for this iteration
        handle_setheaders(h, "accept" = "application/x-bibtex")

        curl::curl(url, handle = h) %>%
            readLines(warn = FALSE) %>%
            write(file = outFile, append = TRUE)
    })
    #try(curl_download(url, destfile = , handle = h, mode = "a"))
}

doi_df  <- bib2df("curltest.bib")
doi_df2 <- bib2df("bib_extra.bib")
nrow(doi_df2)
doi_df2$YEAR %>% unique %>% sort()

doi_df2 %>%
    nrow()

doi_df3 <- bind_rows(doi_df, doi_df2)

doi_df3$YEAR %>% unique %>% sort()

View(doi_df3)

doi_df4 <- doi_df3 %>% 
    distinct(TITLE, .keep_all = TRUE) 
    
doi_df4 %>% 
    nrow()

doi_df4$CATEGORY %>% unique

doi_df4 %>%
    group_by(CATEGORY) %>%
    summarise(n = n())
    #filter(!(CATEGORY %in% c("PHDTHESIS", "PATENT", "BACHELORSTHESIS", "MASTERSTHESIS"))) %>%
    #nrow()

doi_df_final <- bib2df("merged_literature.bib")
doi_df_final$MONTH %>% unique

doi_df_final %>%
    group_by(CATEGORY) %>%
    summarise(n = n())

missing_titles <- setdiff(titlelist, doi_df3$TITLE)
missing_rows <- bib_df %>% filter(TITLE %in% missing_titles)
missing_rows %>% nrow()
missing_rows$DOI

bib_df %>% 
    filter(DOI == "") %>%
    pull(TITLE)
