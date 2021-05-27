# library(tm)
# remomve numbers
# X$tx <- removeNumbers(
#   as.character(X$tx)
# )

##### POS Tagging ############################################
(n.cores = detectCores())
clust <- makeCluster(n.cores)
registerDoParallel(n.cores)
t0 = Sys.time()
D = foreach(i = 1:length(TXN), .packages=c('udpipe')) %dopar% {
  en_model <- udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")  
  df = udpipe_annotate(en_model, TXN[[i]])
  df = data.frame(df)
  df$chunk = i
  df
}
Sys.time() - t0     # 34.154 mins -> 8.599 mins
stopCluster(clust)

##### Combind ################################################
(offset = c(0, sapply(TXN, length) %>% cumsum)[1:K] %>% setNames(1:K))
for(i in 1:length(offset)) {
  D[[i]]$id = offset[i] + as.integer(str_remove(D[[i]]$doc_id,"^doc")) }
D = do.call(rbind, D)
c(n_distinct(D$id), n_distinct(D$id, D$paragraph_id), 
  n_distinct(D$id, D$paragraph_id, D$sentence_id))    # 2195  2195 13042
D$id = as.integer(D$id)
D$doc_id = paste0("doc",D$id)
identical(unique(D$id), 1:max(D$id)) # TRUE

UD = D[,c(16,2,3,5:9)]   # 3,244,030/3,441,271 tokens
# S = D %>% group_by(id, paragraph_id, sentence_id) %>%
S = D %>% group_by(id, paragraph_id) %>%
  summarise(tx = sentence[1]) %>% 
  data.frame             # 118,105/132,054 sentences
POS = D
# save(X, UD, S, file="UD1900sen.rdata")
# save(X, UD, S, file="udPOS20.rdata")
# save(X, UD, S, file="test.rdata")