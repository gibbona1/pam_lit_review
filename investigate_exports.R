folder <- "C:/Users/Anthony"

#Scopus
df1 <- read.csv(file.path(folder, "Downloads", "scopus.csv"))
colnames(df1)
df1[1,]
df1$Abstract[1]
df1[1,c("Title", "Year", "Abstract", "Index.Keywords")]

#Google Scholar (using Publish or Perish)
df2 <- read.csv(file.path(folder, "Downloads", "PoPCites.csv"))
colnames(df2)
df2[1,]
df2$Abstract[1]
df2[1, c("Title", "Year", "Abstract")]

library(readxl)
#Web of Science
df3 <- read_excel(file.path(folder, "Downloads", "savedrecs.xls"))
colnames(df3)
df3[1,]
df3$Abstract[1]
df3[1,c("Publication Year", "Article Title", "Abstract", "Author Keywords")]
