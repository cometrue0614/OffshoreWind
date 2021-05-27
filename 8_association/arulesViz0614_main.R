rm(list=ls(all=T))
pacman::p_load(slam, tm, Matrix, dplyr, arules, arulesViz, visNetwork, sprof, lattice, data.table, clickstream)

load("~/R_Workspace/MIRDC/V20/workflow/2_tagging/udPOS20.rdata")

table(UD$upos)
df = UD %>% 
  filter(upos %in% c("NOUN","PROPN","VERB","ADJ","ADV")) %>%
  filter(!lemma %in% stopwords("en")) 

# save(df, file = "df.rdata")
load("C10a.rdata")
gi = group_indices(X, title, tx)
list = split(df$lemma, gi)
names(list) = paste0("Tr", c(1:length(list)))

TX = lapply(list, iconv, from = "latin1", to = "ASCII", sub = "_") %>% as("transactions")
inspect(TX[1:5])
# save(TX, file = "TX.rdata")
itemFrequency(TX) %>% sort(decreasing = T) %>% head(100)
summary(TX)   # 106938 transactions, 2593 items
itemFrequency(TX) %>% sort(decreasing=T) %>% head(100)

# ix = itemFrequency(TX) %>% {names(.)[. < 5/nrow(S)]}
# ix = c(ix, "wind","offshore","project","say","also","use","include","well",
#        "take","mappress","next")
# 
# df = subset(df, !lemma %in% ix) 
# gi = group_indices(X, title, sentence)



##### query for association rules (high threshold)
rules = apriori(TX, parameter = list(support = 0.03, confidence = 0.03))
rules  # 707642 rules
# save(rules, file = "rules3.rdata")

arules::subset(rules, subset =
                 lhs %pin% "vessel" &
                 support > 0.0004 &
                 confidence > 0.5)
