rm(list=ls(all=T)); gc()

pacman::p_load(doParallel,foreach,udpipe,stringr,dplyr)
load("~/R_Workspace/MIRDC/V20/workflow/1_preProcessing/0118news.rdata") # load original data
# X = read_csv("~/thesis/data/corpus_1900sen.csv")

##### Split ################################################
# X = X[ !is.na(X$date),]
# write_csv(X, "~/thesis/data/news.csv")
# tx = X$tx
tx = X$corpus
K = 10; N = length(tx)/K
TXN = split(tx, ceiling(seq_along(tx)/N))

