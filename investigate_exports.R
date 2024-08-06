folder <- "C:/Users/Anthony"
cnames <- c("Year", "Title", "Abstract", "Keywords")

#Scopus
df1 <- read.csv(file.path(folder, "Downloads", "scopus.csv"))
colnames(df1)
df1[1,]
df1$Abstract[1]
df1_sub <- df1[, c("Year", "Title", "Abstract", "Index.Keywords")]
colnames(df1_sub) <- cnames

#Google Scholar (using Publish or Perish)
df2 <- read.csv(file.path(folder, "Downloads", "PoPCites.csv"))
colnames(df2)
df2[1,]
df2$Abstract[1]
df2_sub <- df2[, c("Year", "Title", "Abstract")]
df2_sub <- cbind(df2_sub, data.frame("keywords"=""))
colnames(df2_sub) <- cnames


library(readxl)
#Web of Science
df3 <- read_excel(file.path(folder, "Downloads", "savedrecs.xls"))
colnames(df3)
df3[1,]
df3$Abstract[1]
df3_sub <- df3[,c("Publication Year", "Article Title", "Abstract", "Author Keywords")]
colnames(df3_sub) <- cnames


df_all <- rbind(df1_sub, df2_sub, df3_sub)

library(ggplot2)

ggplot(df_all, aes(x=Year)) +
  geom_histogram(stat = "count")

