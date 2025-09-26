# Lasting av nødvendige R-pakker 
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(readtext)
library(ggplot2)
require(readtext)
library(tidyverse)

# Definere arbeidsmappe (her spesifiserer stien til mappen på din maskin!)
setwd("~/Github/NPT-tekstanalyser")

# Lasting av datafiler
mine_data <- readtext("Data/*.txt",
                    docvarsfrom = "filenames",
                    dvsep = "-",
                    docvarnames = c("fase", "gruppe"))

# Omkoding av gruppe-variablene
mine_data$gruppe <- ifelse(mine_data$gruppe == "forskere", "Forskere", "Lærere")
mitt_corpus <- corpus(mine_data$gruppe)

# Generering av data-corpus
mitt_corpus <- corpus(mine_data)
summary(mitt_corpus)

# Klargjøring av data til videre analyser
dfm_data <- mitt_corpus %>% 
  tokens(what = "word", 
         remove_punct = TRUE, 
         remove_numbers = TRUE, 
         remove_symbols = TRUE) %>% 
  tokens_select(stopwords('norwegian'),
                selection='remove') %>% 
  dfm()

dfm_data <- dfm_group(dfm_data, groups = dfm_data$group)
dfm_data <- dfm_remove(dfm_data, c("jo", "5", "1", "eh", "eeh", "mm", "mhm", "mmm", "mmmm", "nei", "uhørbart", "hehe", "to", "ti", "4", "2", "3", "få", "okey", "ok", "okei", "jaja", "oi", "x-2", "fillen", "forsker1", "forsker2", "forsker3", "rosie", "roy"))

# Relativ frekvens analyse med fokus på forskere
forskere_keyness <- textstat_keyness(dfm_data, target = 1)
textplot_keyness(forskere_keyness, n=20)

corpus_keyness_forskere <- textstat_keyness(dfm_data, target = 1)
corpus_keyness_lærere <- textstat_keyness(dfm_data, target = 2)

# Forarbeide til leksikonbaserte analyser
corp_forskere <- corpus_subset(mitt_corpus, gruppe %in% c("Forskere"))
corp_lærere <- corpus_subset(mitt_corpus, gruppe %in% c("Lærere"))

complete_lærere <- tokens(corp_lærere, what = "word", 
                          remove_punct = TRUE, 
                          remove_numbers = TRUE, 
                          remove_symbols = TRUE)
complete_lærere <- tokens_select(complete_lærere, stopwords('norwegian'), selection='remove')
complete_lærere_dfm <- dfm(complete_lærere)

complete_forskere <- tokens(corp_forskere, what = "word", 
                          remove_punct = TRUE, 
                          remove_numbers = TRUE, 
                          remove_symbols = TRUE)
complete_forskere <- tokens_select(complete_forskere, stopwords('norwegian'), selection='remove')
complete_forskere_dfm <- dfm(complete_forskere)


# Lese inn ordbok-filer
faglige_ord <- readLines("Ordbok/faglige.txt")
modifiserende_ord <- readLines("Ordbok/modifiserende.txt")
forsterkende_ord <- readLines("Ordbok/absolutter.txt")
partnerskaps_ord <- readLines("Ordbok/partnerskap.txt")
fortellings_ord <- readLines("Ordbok/fortellinger.txt")

# Definere ordbøker
communication_dictionary <- dictionary(list(faglig = faglige_ord,
                                            modifiserende = modifiserende_ord,
                                            forsterkende = forsterkende_ord,
                                            partnerskap = partnerskaps_ord,
                                            fortellinger = fortellings_ord))

dict_dfm_forskere <- dfm_lookup(complete_forskere_dfm, communication_dictionary, nomatch = "unmatched")
dict_dfm_forskere

dict_dfm_lærere <- dfm_lookup(complete_lærere_dfm, communication_dictionary, nomatch = "unmatched")
dict_dfm_lærere

# Nettverksanalyser med fokus på forskerne
complete_forskere_dfm <- dfm_remove(complete_forskere_dfm, c("jo", "5", "1", "eh", "eeh", "mm", "mhm", "mmm", "mmmm", "nei", "uhørbart", "hehe", "to", "ti", "4", "2", "3", "få", "okey", "ok", "okei", "jaja", "oi", "x-2", "fillen", "forsker1", "forsker2", "forsker3", "rosie", "roy"))
forskere_top <- head(corpus_keyness_forskere$feature, 50)
fcmat_forskere <- fcm(complete_forskere_dfm)
fcmat_forskere <- fcm_select(fcmat_forskere, pattern = forskere_top)
textplot_network(fcmat_forskere, min_freq = 0.1, edge_alpha = 0.5, edge_size = 1, vertex_labelsize = 8)

# Nettverksanalyser med fokus på lærerne
complete_lærere_dfm <- dfm_remove(complete_lærere_dfm, c("jo", "5", "1", "eh", "eeh", "mm", "mhm", "mmm", "mmmm", "nei", "uhørbart", "hehe", "to", "ti", "4", "2", "3", "få", "okey", "ok", "okei", "jaja", "oi", "x-2", "fillen", "forsker1", "forsker2", "forsker3", "rosie", "roy"))
lærere_top <- head(corpus_keyness_lærere$feature, 50)
fcmat_lærere <- fcm(complete_lærere_dfm)
fcmat_lærere <- fcm_select(fcmat_lærere, pattern = lærere_top)
textplot_network(fcmat_lærere, min_freq = 0.1, edge_alpha = 0.5, edge_size = 1, vertex_labelsize = 8)
