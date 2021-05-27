pacman::p_load(stringr,dplyr)
# load("~/R_Workspace/MIRDC/V20/workflow/2_tagging/udPOS20.rdata")
# load("~/R_Workspace/MIRDC/V20/workflow/2_tagging/test.rdata")
load("~/R_Workspace/MIRDC/V20/workflow/2_tagging/UD1900sen.rdata")
rm(UD); gc()
# E = read.csv("~/R_Workspace/MIRDC/V20/workflow/3_C20/E20.csv", stringsAsFactors=F) # 654 7
# E = read.csv("~/thesis/data/E_O_V.csv", stringsAsFactors=F) # 1082 7

mx = sapply(1:nrow(E), function(i) str_detect(
  S$tx, regex(E$alias[i], ignore_case = E$ignore[i])))

dim(mx) # 132054(總計句子數) -> 135606, 654(Entity的數量)
colnames(mx) = E$name
E$freq = colSums(mx); range(E$freq) # 6, 14203 ->  4, 14689
i = order(E$class, -colSums(mx))
E = E[i,]; mx = mx[,i]

# E[E$freq < 10,] %>% View
# E = subset(E, freq >=10)
table(E$class=="ORG", is.na(E$sub_class))
#       FALSE TRUE
# FALSE     0  464
# TRUE    192    0
# write.csv(E, "~/thesis/data/E_O_V.csv", row.names=F, quote=T)
# write.csv(E, "~/R_Workspace/MIRDC/V20/workflow/3_C20/E20.csv", row.names=F, quote=T)

xSent = mx
xPara = t(sapply(
  split(data.frame(mx), group_indices(S, id, paragraph_id)),
  colSums)) > 0
xDocu = t(sapply(split(data.frame(mx), S$id), colSums)) > 0

XX = list(Sent=xSent, Para=xPara, Docu=xDocu)
CO = list(Sent = t(xSent) %*% xSent,
          Para = t(xPara) %*% xPara,
          Docu = t(xDocu) %*% xDocu )
CR = list(Sent = cor(xSent),
          Para = cor(xPara),
          Docu = cor(xDocu))

# X$tx[is.na(X$tx)] = ""
# z = "\\bROV|AUV\\b"
# z = "\\b(C|c)orrosion|(S|s)acrificial\\b"
# i = str_detect(X$tx, z)
# x = XX$Docu[i, E$class=="ORG"]
# x = x[,colSums(x) > 0]
# 
# table(x, useNA='ifany')
# x = XX$Docu[i, E$class=="ORG"] %>% cor
# x[is.na(x)] = 0
# hist(x, useNA='ifany')

save(X, E, S, XX, CO, CR, file="~/R_Workspace/MIRDC/V20/workflow/3_C20/C20.rdata", compress=T)









