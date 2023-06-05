files.v <- dir(pattern = "xlsx")


zzz <-  read.csv(file = "UTF_labeSonnetsModernSpelling.csv")

op <- gsub(".csv", ".RDS", files.v[1])
saveRDS(working.df, file = op )



# create sample data frame
df <- data.frame(Name=c('Priyank Mishra', 'Abhiraj Srivastava',
                        'Pawananjani Kumar'),
                 State= c("Uttar Pradesh", "Maharashtra", "Bihar"))

print(" Data frame before splitting: ")
df

# load dplyr and tidyr library
library(dplyr)
library(tidyr)

# Split name column into firstname and last name
df <- df %>% separate(Name, c('First Name', 'Last Name'))

print(" Data frame after splitting: ")
df

working.df$feats %>% strsplit("\\|")

zz <- working.df %>% separate(col = feats, into = c("a", "b", "c", "d", "e"), sep = "\\|")


zz[, 14:18] 
zz[, c("Number", "Gender", "VerbForm", "Tense")] <- NA


zzz <- read_xlsx(files.v[1])

working.df$sentence[1:5]

sample.df <- working.df
target.df[1:25, 50:75] %>% View()

ccc <- which(str_detect(colnames(target.df), "ISA"))

ssss <- target.df[, ccc] %>% colSums()

summary(ssss)/nrow(target.df)
