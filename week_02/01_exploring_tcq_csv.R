# install and load packages

#install.packages("tidyverse")
library(tidyverse)
library(readr)



# read in csv to "data_raw" variable

data_raw <- read_csv("data/tcq_wos_data.csv")



## take a high-level look at the data

glimpse(data_raw)



## use janitor library to clean the names

#install.packages("janitor")
library(janitor)
help("janitor") # use help("library_name") for a description



# here, we create a new variable from the result of running the clean names function on our raw data
data_cleannames <- data_raw %>% clean_names()



# see the result: column names are now lowercase and words are joined with underscores
glimpse(data_cleannames)



## explore at a high level

dim(data_cleannames) # shows dimensions (number of rows and columns)

summary(data_cleannames) # provides overview of numeric variables

summary(data_cleannames$publication_year)

str(data_cleannames) # for each column, shows data types and first few observations

head(data_cleannames) # shows first few rows

tail(data_cleannames) #shows last few rows

## explore specific columns

head(data_cleannames$article_title) # this construction allows us to isolate a column: "data$column_name" 

table(data_cleannames$publication_year) # creates a frequency table for a categorical variable 


## visualize 

ggplot(data_cleannames, aes(x = publication_year)) +
  geom_bar() +
  labs(title = "Bar Plot of Publication Year")

pub_year_barplot <- ggplot(data_cleannames, aes(x = publication_year)) +
  geom_bar() +
  labs(title = "Bar Plot of Publication Year")

pub_year_barplot

pub_year_barplot <- pub_year_barplot +
  theme_light() +  # add a theme
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 4) +  # add labels above the bars
  labs(
    title = "Web of Science: Articles in TCQ, by year",
    x = "Publication Year",
    y = "Number of Articles"
  ) # define chart labels

pub_year_barplot

## save the plot 
help(ggsave)
ggsave("plots/tcq_pub_years_barplot.png", 
       plot = pub_year_barplot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       bg = NULL)

# Examine titles and abstracts

head(data_cleannames$article_title)

head(data_cleannames$abstract)

## create a new column that combines the title and abstract

# Using paste() to combine the two columns with a tilde separator
data_clean <- data_cleannames %>%
  mutate(title_abstract = paste(article_title, abstract, sep = " ~ ")) 

head(data_clean$title_abstract)

# Analyzing words in titles and abstracts

install.packages("tidytext")
library(tidytext)

# create a new dataframe in which each word-year pairing gets it's own row
ta_words_year <- data_clean %>%
  unnest_tokens(word, title_abstract) %>%
  count(publication_year, word, sort = TRUE)

head(ta_words_year)

ta_words_total <- ta_words_year %>%
  group_by(publication_year) %>%
  summarize(total = sum(n))

head(ta_words_total)
  
ta_words_year <- left_join(ta_words_year, ta_words_total)

head(ta_words_year, n = 20)

tail(ta_words_year, n = 20)

## Visualize the distribution of words

ggplot(ta_words_year, aes(n/total, fill = publication_year)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~publication_year, ncol = 2, scales = "free_y")

