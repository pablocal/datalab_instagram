# Metadata ----------------------------------------------------------------
# Title: hackaton-netcoin.R
# Purpose: Hackaton netCoin
# Author(s): @pablocal
# Date: 2020-10-16 17:20:19
#
# Comments ----------------------------------------------------------------
#
#
#
#
#
# Options and packages ----------------------------------------------------
library(tidyverse)
library(tidytext)
library(netCoin)
library(topicmodels)

d <- openxlsx::read.xlsx("1_data/data.xlsx")


# generate a hashtag list -------------------------------------------------
d %>% 
select(caption_hashtags) %>% 
mutate(hashtag = str_remove_all(caption_hashtags, "\\["),
       hashtag = str_remove_all(hashtag, "\\]"),
       hashtag = str_remove_all(hashtag, "#"),
       hashtag = str_trim(hashtag, "both")
       ) %>% 
separate(hashtag, letters[1:25]) %>% 
pivot_longer(a:y, names_to = "var", values_to = "hash") %>% 
filter(!is.na(hash) & hash != "") %>% 
pull(hash)  %>% 
unique() %>% 
tibble(hashtag = ., include = 0) %>% 
write_csv("1_data/hash_list.csv")


hashtag_list <- read_csv("1_data/hash_list_inc.csv") %>% 
        filter(include == 1) %>% 
        select(hashtag) %>% 
        write_csv("1_data/hash_list_final.csv")

# recodes and trans
rec_scale <- function(x){
 x <- str_replace_all(x, "_", " ")
 x <- str_to_sentence(x)
 x <- factor(x, levels = c("Very likely", 
                           "Likely", 
                           "Possible", 
                           "Unlikely", 
                           "Very unlikely"))
}

d <- d %>% 
 mutate_at(vars(medical, spoof, violence, adult, racy), 
           list(fct = rec_scale,
                 d = ~ifelse(. %in% c("UNLIKELY", "VERY_UNLIKELY"), 0, 1),
                 d2 = ~ifelse(. %in% c("VERY_UNLIKELY"), 0, 1)
                 )
           ) %>% 
 mutate(week = lubridate::week(date),
        month = lubridate::month(date),
        year = lubridate::year(date)
        )
                  
 
# spoof and racy are present more variability; try violence as well. 
d %>% 
 select(ends_with("_fct"), ends_with("_d")) %>% 
 sjmisc::frq()

# better try with dummies
sjmisc::flat_table(d, spoof, racy, margin = "count")
sjmisc::flat_table(d, spoof, racy, margin = "col")

# few cases for racy and violence
sjmisc::flat_table(d, spoof_d, racy_d, margin = "count")
sjmisc::flat_table(d, spoof_d, racy_d, margin = "col")
sjmisc::flat_table(d, spoof_d, violence_d, margin = "count")
sjmisc::flat_table(d, spoof_d, violence_d, margin = "col")
sjmisc::flat_table(d, racy_d, violence_d, margin = "count")
sjmisc::flat_table(d, racy_d, violence_d, margin = "col")

# relat. time and spoof_d
d %>% 
 filter(year > 2014) %>%
 group_by(year, month) %>% 
 summarise(n = n(),
           spoof = sum(spoof_d)) %>% 
 ungroup() %>% 
 mutate(ym = as.Date(paste0("01/", month, "/", year),format = "%d/%m/%Y"), 
        p_spoof = spoof/n) %>% 
 ggplot(aes(ym, p_spoof, group = factor(1))) +
 geom_line() +
 geom_smooth()

# relat. time and all *_d2
d %>% 
 select(year, month, ends_with("_d2")) %>% 
 pivot_longer(ends_with("_d2"), 
              names_to = "indicator",
              values_to = "val") %>% 
 group_by(year, month, indicator) %>% 
 summarise(n = n(),
           ind = sum(val)) %>% 
 ungroup() %>% 
 mutate(ym = as.Date(paste0("01/", month, "/", year),format = "%d/%m/%Y"),
        p_ind = ind/n) %>% 
 ggplot(aes(paste0(year, "-", month), p_ind, group = factor(1))) +
 geom_line() +
 facet_wrap(~ indicator, ncol = 1) +
 theme(axis.text.x = element_text(angle = 75))


# terms network -----------------------------------------------------------
txt <- d %>% 
        select(code, caption) %>% 
        unnest_tokens(word, caption) %>%
        anti_join(get_stopwords()) %>% 
        filter(!is.na(word)) %>% 
        filter(!(word %in% as.character(1:2020))) %>% 
        filter(word != "code")

words <- txt %>% 
        group_by(word) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        filter(count > 40) %>% 
        filter(!(word %in% as.character(1:2020))) %>% 
        pull(word)

txt_coin <- txt %>% 
        group_by(code, word) %>% 
        summarise(count = n()) %>% 
        ungroup()  

smpl_word <- sample(unique(txt_coin$word), 250)        
        
txt_coin_wide <- txt_coin %>% 
        mutate(val = ifelse(count > 0, 1, 0)) %>% 
        select(-count) %>% 
        filter(word %in% smpl_word) %>%
        pivot_wider(names_from = word,
                    values_from = val,
                    values_fill = 0,
                    names_repair = "unique") 


# 2. Network of terms -----------------------------------------------------
c <- coin(txt_coin_wide[,-1])
nodes <- asNodes(c)
edges <- edgeList(c)
net <- netCoin(nodes, edges, community = "lo")
plot(net)

# 3. Scatterplot with surcoin ---------------------------------------------
scat2 <- netCoin::surScat(txt_coin_wide[,-1], nclusters = 2)
scat3 <- netCoin::surScat(txt_coin_wide[,-1], nclusters = 3)
scat4 <- netCoin::surScat(txt_coin_wide[,-1], nclusters = 4)
scat5 <- netCoin::surScat(txt_coin_wide[,-1], nclusters = 5)
scat6 <- netCoin::surScat(txt_coin_wide[,-1], nclusters = 6)

plot(scat2)
plot(scat3)
plot(scat4)
plot(scat5)
plot(scat6)

# 4. Topic analysis -------------------------------------------------------
dtm <- cast_dtm(txt_coin, code, word, count)

ap_lda2 <- LDA(dtm, k = 2, control = list(seed = 1234))
ap_lda3 <- LDA(dtm, k = 3, control = list(seed = 1234))
ap_lda4 <- LDA(dtm, k = 4, control = list(seed = 1234))
ap_lda5 <- LDA(dtm, k = 5, control = list(seed = 1234))
ap_lda6 <- LDA(dtm, k = 6, control = list(seed = 1234))


ap_topics <- tidy(ap_lda6, matrix = "beta")

ap_top_terms <- ap_topics %>%
        group_by(topic) %>%
        top_n(20, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)

ap_top_terms %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()

