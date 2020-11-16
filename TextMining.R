#==============================================================================
# November 10th, 2020

# In this script, I will be playing around with the various methods in the 
# text mining packages in R. I will explore the versatility of packages tm
# quanteda, dplyr, tidyr, etc

#==============================================================================
# LIBRARIES
install.packages('tidytext')
install.packages('janeaustenr')
install.packages('stringr')
install.packages("gutenbergr")
install.packages("textdata")
library(janeaustenr)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(gutenbergr)
library(tidyr)
library(scales)
library(textdata)

# Define a new variable containing strings, we'll name it text
# An except from Maya Angelou's And Still I Rise
text <- c( "You may write me down in history",
           "With your bitter, twisted lies",
           "You may trod me with the very dirt",
           "But still, like dust, I rise",
           "Just like the moon and suns",
           "With the certainty of tides",
           "Just like hopes and springing high",
           "I rise"
  )
# A quick side note on tibbles: available in dplyr and tibble packages
# Useful for printing out the data on to the console; will not convert
# strings to factors; doesn't use row names great for tidy tools
text_df <- tibble(line = 1:8, text = text)

# # A tibble: 8 x 2
# line text                              
# <int> <chr>                             
# 1     1 You may write me down in history  
# 2     2 With your bitter, twisted lies    
# 3     3 You may trod me with the very dirt
# 4     4 But still, like dust, I rise      
# 5     5 Just like the moon and suns       
# 6     6 With the certainty of tides       
# 7     7 Just like hopes and springing high
# 8     8 I rise 

# But the table above is not in tidy text format

# Now we apply tokenization, a process where we break the text into individual
# tokens and convert to a tidy data structure.

# library(tidytext)
# To suppress conversion to lower case, use: to_lower = FALSE argument
text_df %>%
  unnest_tokens(word, text)

# A tibble: 45 x 2
# line words  
# <int> <chr>  
# 1     1 you    
# 2     1 may    
# 3     1 write  
# 4     1 me     
# 5     1 down   
# 6     1 in     
# 7     1 history
# 8     2 with   
# 9     2 your   
# 10     2 bitter 

# Having text in this format allows us to manipulate, process, and to visualize
# the data using the standard tidy tools dplyr, tidyr, and ggplot2

# FLOW CHART OF THE DATA MINING PROCESS
#
#         unnest_tokens()           count
#            tidytext               dplyr                ggplot2
# TEXT DATA     --->   TIDY TEXT    --->    SUMMARIZE     --->    VISUALIZATION
#                                             TEXT
#                    dplyr, tidyr          dplyr,tidyr
#
#
# The janeaustenr package comes with some books in tidy form, each row represents
# a book.
#
# Let's load this package into R and work with it
#
# library(janeaustenr)
# library(dplyr)
# library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(text,
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = T)))) %>%
  ungroup()

# This text data needs to be restructured: tokenized

# library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

# This command uses the tokenizers package that separates each line of the txt 
# in the original document into the various tokens. Default tokenizing is for 
# words, but other oprions are possible: sentences, n-grams, paragraphs, 
# characters, lines, or separation around a regex pattern

# book               linenumber chapter word      
# <fct>                   <int>   <int> <chr>     
# 1 Sense & Sensibili~          1       0 sense     
# 2 Sense & Sensibili~          1       0 and       
# 3 Sense & Sensibili~          1       0 sensibili~ 
# 4 Sense & Sensibili~          3       0 by        
# 5 Sense & Sensibili~          3       0 jane      
# 6 Sense & Sensibili~          3       0 austen    
# 7 Sense & Sensibili~          5       0 1811      
# 8 Sense & Sensibili~         10       1 chapter   
# 9 Sense & Sensibili~         10       1 1         
# 10 Sense & Sensibili~         13       1 the 

# Now the data is in one-word-per-row format; we can use it now

#============================== STOP WORDS ====================================
# Words that are not useful to an analysis: words like "to" "the" "of"
# we want to remove these

# Stop words are in the tidy dataset stop_words, and we can remove them with an 
# anti_join()

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

# stop_words contains stop words from three lexicons, we can use filter() to
# use only one of the set of stop words for appropriate analysis

# We can also use dplyr's count() to see the most common words in all the 
# data

tidy_count <- tidy_books %>%
  count(word, sort = T)


# # A tibble: 13,914 x 2
# word       n
# <chr>  <int>
# 1 miss    1855
# 2 time    1337
# 3 fanny    862
# 4 dear     822
# 5 lady     817
# 6 sir      806
# 7 day      797
# 8 emma     787
# 9 sister   727
# 10 house    699
# # ... with 13,904 more rows

# Because we are using tidy tools, word counts are stored in a data frame
# This will help us plot them directly with ggplot

# library(ggplot2)
#====
# Start an output device to display plot
pdf(file = "countPLOT.pdf", width = 10, height = 6, title = "Word count in book")

tidy_books %>%
  count(word, sort = T) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word))+
  geom_col()+
  labs(y=NULL)

# Save the plot
dev.off()
# Word Frequencies 
# We add an additional library of works, the the gutenbergr package which contains public 
# domain works from the Project Gutenberg

# library(gutenbergr)
# We can access works using gutenberg_dowload() method and the Project Gutenberg ID numbers 
# for each piece of work

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# Convert the text data into tidy form 
 tidy_hgwells <- hgwells %>%
   unnest_tokens(word, text) %>%
   anti_join(stop_words)
 
# Now check the most common words across works
tidy_hgwells %>%
  count(word, sort = T)

# Bronte Sister's works
bronte <- gutenberg_download(c(1260, 768, 969, 982, 767))

# Convert to tidy form
tidy_bronte <- bronte %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

# Check for word frequencies
tidy_bronte %>%
  count(word, sort = T)

# Now we wish to compute the frequency of each word for the works of Jane Austen, H.G. Wells, 
# and and bronte sisters by bunding the data frames together

# We will use spread() and gather() from tidyr to reshape our data frame so that 
# it is just what we want for plotting and comparing the three sets of novels

# library(tidyr)
freq_word <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z]+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Bronte Sisters`:`H.G. Wells`)

# Now let's plot these frequencies
# I've loaded the data into my console and have seen that some rows have 
# missing values

# Start an output device to display plot
pdf(file = "janeaustenComparison.pdf", width = 10, height = 6, title = "Word count in book")

ggplot(freq_word, aes(x = proportion, y = `Jane Austen`,
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = .1, size = 2.5, width = .3, height = .3) +
  geom_text(aes(label = word), check_overlap = T, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position = "none") +
  labs(y = "Jane Austen", x = NULL)

dev.off()

# Now we quantify how similar and different the sets of text are 
# We will use correlation test between the three set of text

cor.test(data = freq_word[freq_word$author == "Bronte Sisters",],
         ~proportion + `Jane Austen`)

# Pearson's product-moment correlation
# 
# data:  proportion and Jane Austen
# t = 119.04, df = 9635, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.7633340 0.7794992
# sample estimates:
#       cor 
# 0.7715411 

cor.test(data = freq_word[freq_word$author == "H.G. Wells",],
         ~proportion + `Jane Austen`)

# Pearson's product-moment correlation
# 
# data:  proportion and Jane Austen
# t = 35.91, df = 6027, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.3988004 0.4403931
# sample estimates:
#       cor 
# 0.4198171





  








