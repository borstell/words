# Words

This repository contains some data on English nouns and their frequencies.

- The words were collected from <https://github.com/ScriptSmith/topwords> (CC-BY-SA-4.0), originally sourced from Project Gutenberg.
- The words were then processed in R with [{udpipe}](https://bnosac.github.io/udpipe/en/) to extract nouns only and tag their number feature (singular/plural).
- Additional processing ensures that only paired lemmas (singular + plural forms) are present, joined with their frequencies.

## Processing script

```r

# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidytext)
library(udpipe)



# Get word frequencies from Github ----------------------------------------
# From: https://github.com/ScriptSmith/topwords (CC-BY-SA-4.0)

all_words <- 
  read_delim("https://raw.githubusercontent.com/ScriptSmith/topwords/refs/heads/master/counts.txt", 
             delim = " ",
             col_names = c("frequency", "word"))



# Process data ------------------------------------------------------------

# Filter out stop words, words with special characters 
# and short and low-frequency words
words <- 
  all_words |> 
  anti_join(tidytext::stop_words) |> 
  filter(frequency > 1000) |> 
  filter(grepl("^[a-z]+$", word)) |> 
  filter(nchar(word) > 2)

# Get UD model for English
en_model <- 
  udpipe::udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# Tag all words and filter to nouns only
nouns <- 
  udpipe_annotate(en_model, 
                  x = pull(nouns, word), 
                  parser = "none") |> 
  as_tibble() |> 
  filter(upos == "NOUN")

# Clean nouns and join with word frequencies
clean_nouns <- 
  nouns |> 
  select(token, lemma, feats) |> 
  mutate(number = case_when(
    str_detect(feats, "Sing") ~ "singular",
    str_detect(feats, "Plur") ~ "plural",
    .default = NA
  )) |> 
  drop_na(number) |> 
  select(-feats) |> 
  inner_join(words, by = join_by(token == word)) |> 
  add_count(lemma) |> 
  filter(n == 2) |> 
  select(-n) |> 
  filter(length(unique(number)) == 2, .by = lemma)

clean_nouns |> 
  select(token, number, frequency) |> 
  write_tsv("token_freqs.tsv")

clean_nouns |> 
  select(word = token, lemma) |> 
  write_csv2("word_lemma.csv")
```
