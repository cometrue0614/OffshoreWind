rm(list=ls(all=T))
pacman::p_load(slam, tm, Matrix, dplyr, arules, arulesViz, visNetwork)
load("data/UD.rdata")

table(UD$upos)
df = UD %>% 
  filter(upos %in% c("NOUN","PROPN","VERB","ADJ","ADV")) %>%
  filter(! lemma %in% stopwords("English")) 

gi = group_indices(df, doc_id, paragraph_id, sentence_id)
TX = as(split(df$lemma, gi), "transactions") 
itemFrequency(TX) %>% sort(decreasing=T) %>% head(100)
ix = itemFrequency(TX) %>% {names(.)[. < 5/nrow(S)]}
ix = c(ix, "wind","offshore","project","say","also","use","include","well",
       "take","mappress","next")

df = subset(df, ! lemma %in% ix) 
gi = group_indices(df, doc_id, paragraph_id, sentence_id)
TX = as(split(df$lemma, gi), "transactions") 
summary(TX)   # 110558 transactions, 13491 items
itemFrequency(TX) %>% sort(decreasing=T) %>% head(100)

##### query for association rules (high threshold)
rules = apriori(TX, parameter = list(support = 0.05, confidence = 0.05))
rules  # 856739 rules

##### get association rules with vessel in the lhs
subset(rules, subset = lhs %pin% "vessel")  # 3700 rules
rx = subset(rules, subset = 
         lhs %pin% "vessel" & 
         support>0.0004 & 
         confidence>0.5
       ) # 31 rules

# plot(rx, method="graph",engine="interactive") 
# plot(rx, method="graph",engine="htmlwidget")

##### save plot, adjust labels
vx = plot(rx, method="graph",engine="htmlwidget", shading="lift")
vx$x$nodes$label =  as.character(vx$x$nodes$label)
vx$x$nodes$label[vx$x$nodes$group=="rule"]=""

##### then plot it by visNetwork
off = "function () {this.setOptions( { physics: false } );}"  
visNetwork(vx$x$nodes, vx$x$edges) %>% 
  visEvents(stabilizationIterationsDone=off) %>% 
  visPhysics("forceAtlas2Based") %>% 
  visNodes(font=list(size=32,face="consolas",strokeWidth=2)) %>% 
  visEdges(smooth=T) %>% 
  visOptions(highlightNearest=list(enabled=T,degree= 1))
  
  


