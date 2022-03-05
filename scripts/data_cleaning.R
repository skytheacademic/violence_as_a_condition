setwd("../data/")
`2010.01.01.2022.01.28` <- read.csv("2010-01-01-2022-01-28.csv")
d = `2010.01.01.2022.01.28`
`2010.01.01.2022.01.28` = NULL

library(tidyverse)

a = NULL
a = str_extract_all(d$notes, "Wagner")
a = str_detect(d$notes, regex("Wagner", ignore_case = TRUE))


a = data.frame(a)
a <- tibble::rowid_to_column(a, "id")
d <- tibble::rowid_to_column(d, "id")



b = subset(a, a == "TRUE")

c = left_join(b, d)

write.csv(c, "wagner_violence_data.csv")










a = NULL
a = str_detect(d$notes, regex("", ignore_case = TRUE))


a = data.frame(a)
a <- tibble::rowid_to_column(a, "id")
d <- tibble::rowid_to_column(d, "id")



b = subset(a, a == "TRUE")

c = left_join(b, d)


a$num


a
