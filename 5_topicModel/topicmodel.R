# 整體流程
# 1. 將原文中的關鍵字做替代（offshore wind -> offshore_wind）
# 2. 取出名詞做lda

# 匯入基本套件
options(digits = 10)
pacman::p_load(dplyr,readr,udpipe,text2vec,ggplot2,stringr,tm)

load("~/R_Workspace/MIRDC/V20/workflow/3_C20/C20.rdata")

# 把name改成`_`相連
E$name = gsub(" ","_",E$name)
tx = X$tx

for(i in 1:nrow(E)) {
  tx = str_replace_all(
    tx, regex(E$alias[i], ignore_case=E$ignore[i]), E$name[i])
}
# backup_tx = tx
# tx = gsub("mappress","",tx)

tx_data = data.frame(doc_id = 1:length(X$tx), text = tx, stringsAsFactors = FALSE)
# udmodel <- udpipe_download_model(language = "english")
annotation <- udpipe(tx_data, "~/R_Workspace/MIRDC/V20/workflow/5_topicModel/english-ewt-ud-2.4-190531.udpipe")

offshore = annotation %>% dplyr::select(doc_id, sentence_id, sentence, token_id, token, lemma, upos)
X$doc_id = 1:nrow(X)
# 將替換後的tx與原本的X（去除原本tx欄位）做結合，
offshore2 = merge(offshore, X[, -3], by = "doc_id")
# saveRDS(offshore2, file = "offshore_wind_pos_0203.rds")
save(offshore2, file = "~/R_Workspace/MIRDC/V20/workflow/5_topicModel/offshore2.rdata")
# their = dtm
# ==================================================================
# 篩選只有名詞的字彙
load("~/R_Workspace/MIRDC/V20/workflow/5_topicModel/offshore2.rdata")
# nn
clean_offshore  = offshore2 %>%
  filter(!(token %in% stopwords()) & upos %in% c("PROPN", "NOUN"))

clean_offshore  = offshore2 %>%
  filter(!(token %in% stopwords()) & upos %in% c("PROPN", "NOUN", "VERB"))

library(udpipe)
# 轉換文件字頻表成dtm
x <- document_term_frequencies(clean_offshore, document = "doc_id", term = "token")
dtm = document_term_matrix(x)
dim(dtm)

# 重要！！兩筆是有問題的紀錄，所以建立DT後不見了
setdiff(1:length(X$tx),as.integer(unique(x$doc_id)))

# 基本資料篩除
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 30)
dim(dtm_clean)
# 13986  6930

tfidf <- dtm_tfidf(dtm_clean)
quantile(tfidf)

# 取50%可能會缺少筆數
# dtm_clean <- dtm_remove_tfidf(dtm_clean, cutoff = 0.05)
dim(dtm_clean)
# 13986  4127
save(dtm_clean, file = "~/thesis/data/OWdtm_2019.rdata")
# 封存 為取得每個字的tfidf
# 取得詞頻，tf-idf
# feq = txt_freq(offshore$text)
# tfidf <-dtm_tfidf(dtm_clean)
# hist(tfidf, breaks = "scott")
# 
# tfidf_ranking = sort(tfidf, decreasing = TRUE) %>% as.data.frame
# colnames(tfidf_ranking) = "tfidf"
# tfidf_ranking$key = rownames(tfidf_ranking)
# 
# entity2 =  str_replace_all(entity$name," ","_")
# 
# new_words = filter(tfidf_ranking,!(tfidf_ranking$key %in% entity2))
# new_words = left_join(new_words,feq,by="key")
# 
# summary(new_words$tfidf)
# summary(new_words$freq)
# 
# filterd = filter(new_words,tfidf>0.4 & freq > 100)
# 
# write.csv(filterd,"filter_keyworde.csv")
# ==================================================
# 建立模型，要包含Seed一起跑
{set.seed(12345) 
lda_model = LDA$new(n_topics = 30, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = 
  lda_model$fit_transform(x = dtm_clean, n_iter = 1000,
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)
}

# 主題與字的分佈
topic_word_distr = lda_model$topic_word_distribution

# 取得個主題前 20 重要的字
c = lda_model$get_top_words(n = 20, lambda = 0.5)
save(doc_topic_distr,topic_word_distr, c, file = "~/R_Workspace/MIRDC/V20/workflow/5_topicModel/nn30.rdata")

write.csv(c, "~/R_Workspace/MIRDC/V20/workflow/5_topicModel/top30words.csv")

save(lda_model, dtm_clean, file = "./lda.rdata")

# 取得視覺化的結果
# lda_model$plot(out.dir = './30')


# 複雜度計算，越低越好
perplexity(dtm, topic_word_distribution = lda_model$topic_word_distribution,
           doc_topic_distribution = doc_topic_distr)

# 跑批次挑選主題數
perplexity_df <- data.frame(perplexity = numeric())
topics <- c(1:25)
set.seed(12345)
for (i in topics){
  lda_model = 
    LDA$new(n_topics = i, 
            doc_topic_prior = 0.1, topic_word_prior = 0.01)
  doc_topic_distr = 
    lda_model$fit_transform(dtm_clean, n_iter = 1000, convergence_tol = 0.01, 
                            check_convergence_every_n = 10)
  topic_word_distr_10 = lda_model$topic_word_distribution
  perplexity_df[i,1] <-perplexity(dtm_clean, topic_word_distr_10, doc_topic_distr)
}

perplexity_df$log_per = log(perplexity_df$perplexity)
g <- ggplot(data=perplexity_df, aes(x= as.numeric(row.names(perplexity_df)))) +
  labs(y="Perplexity",x="Number of topics") +
  ggtitle("Perplexity of data") +
  geom_line(aes(y=log_per), colour = "red")
g

# hc = doc_topic_distr %>% dist%>% hclust(method = "ward.D2")
# plot(hc)
# rect.hclust(hc, k=16, border="red")


