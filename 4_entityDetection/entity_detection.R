library(readr)
E20 <- read_csv("~/R_Workspace/MIRDC/V20/workflow/3_C20/E20.csv")  # 字典
View(E20)

# Entity Detection Code Below
##### Org. Entities ###########################################
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load_gh("trinker/entity")
load("~/R_Workspace/MIRDC/V20/workflow/2_tagging/udPOS20.rdata")
oENT = organization_entity(X$tx) # udpipe function (要做很久)
orgs = oENT %>% unlist %>% table %>% sort(decr=T)
save(oENT, file="~/R_Workspace/MIRDC/V20/workflow/4_entityDetection/ENKW.rdata")

##### N-Gram Keywords #########################################
library(udpipe)
UD$lower <- tolower(UD$token)
pmi = keywords_collocation(  # Pointwise Mutual Information (PMI)
  subset(UD, upos %in% c("NOUN","PROPN","VERB","ADJ","ADP")), 
  term = "token", group = c("id","paragraph_id","sentence_id"),
  ngram_max = 5, n_min = 20) # 字數小於5個字，至少出現20次
with(pmi, sum(pmi > quantile(pmi, 0.80) & freq > 30))
save(oENT, pmi, file="ENKW.rdata")

##### N-Phrase Keyworks #######################################
UD$phrase_tag <- as_phrasemachine(UD$upos, type = "upos")
n.phrase = keywords_phrases(
  x = UD$phrase_tag, term = UD$lower, 
  pattern = "(A|N)*N(P+D*(A|N)*N)*", 
  is_regex = TRUE, detailed = FALSE)

save(oENT, pmi, n.phrase, file="ENKW.rdata")

##### Regex Extract #######################################
extr = function(z, ignore = TRUE, n = 1) {
  str_extract_all(X$tx, regex(z, ignore_case = ignore)) %>% 
    unlist %>% table %>% {.[.>=n]} %>% sort }

# 比對是否有新的Entity
# edf = NULL
# edf2 = NULL
# for (i in 1:length(oENT)) {
#   for (j in 1:length(oENT[[i]])) {
#     if (!is.null(oENT[[i]][j])) {
#       if (!oENT[[i]][j] %in% E20$name) {
#         if (!oENT[[i]][j] %in% edf$new) {
#           edf2 = edf2 %>% bind_rows(
#             data.frame(
#               new = oENT[[i]][j]
#             )
#           )
#         }
#       }
#     }
#   }
# }
# 
# for (i in 1:length(pmi)) {
#   if (!pmi$keyword[i] %in% E20$name) {
#     if (!pmi$keyword[i] %in% edf$new) {
#       edf = edf %>% bind_rows(
#         data.frame(
#           new = pmi$keyword[i]
#         )
#       )
#     }
#   }
# }

##### Regex Extract #######################################
extr("(\\S+ ){2}SSE", n=3)
extr("(\\S+ ){4,6}\\(CDWE\\)")
extr("Scottish( &| and)? Southern (Energy|Electric)|\\bSSE\\b")
extr("MHI( |-)Vestas|\\bMHI\\b")

# Find Abbreviations
extr("\\([A-Za-z\\-]{2,6}\\)", n = 4)

extr("(\\S+ ){3,5}\\(VSC\\)", T)
extr("(\\S+ ){4,6}\\(CDWE\\)")

extr("(\\S+ ){4}\\(LC(O|o)E\\)")
extr("Leveli(s|z)ed( |-)Cost( |-)of( |-)(Energy|Electricity)|LCOE",T)

extr("(\\S+ ){4}\\(OWA\\)")
str_subast(S$tx, "Offshore & Wind Assistance")  # it is a company
extr("(O|o)ffshore (W|w)ind (A|a)ccelerator( program)?|\\bOWA\\b",T)

extr("(\\S+ ){3}\\(CfD\\)")
extr("Hollandse Kust")

extr("(\\S+ ){5}\\(EOWDC\\)")
extr("European Offshore Wind De(velop|ploy)ment Cent(re|er)|\\bEOWDC\\b")

extr("(\\S+ ){3}\\(CTV\\)")
extr("(C|c)rew( |-)(T|t)ransfer( |-)(V|v)essels?|\\bCTVs?\\b")

extr("(\\S+ ){4}\\(BOWL\\)")
extr("Beatrice Offshore Wind(farm| Farm)? L\\S+|\\bBOWL\\b")

extr("(\\S+ ){4}\\(DOE\\)")
extr("Department of Energy|\\bD(O|o)E\\b")

extr("Hollandse Kust( \\(zuid\\))?")

extr("(\\S+ ){1,4}((O|o)ffshore )?(W|w)ind(( )?farm| Farm)?", n = 5)

extr("\\bVBMS\\b")
extr("(\\S+ ){4,6}\\(VBMS\\)")

extr("\\bCDWE('s)\\b")
extr("Nordsee Ost", T)

###### Wind Farms ####################################################
tx = X$tx
vextr(tx, "(?<=(W|w))ind( |-)?farm",T)
tx = str_replace_all(tx, regex("(?<=(W|w))ind( |-)?farm", T), "indfarm")
vextr(tx, "(O|o)ff( |-)?shore( |-)(W|w)indfarm")
tx = str_replace_all(tx, "(O|o)ff( |-)?shore( |-)(W|w)indfarm", "OSWF")
vextr(tx, "OSWF|(W|w)indfarm")

vextr(tx, "([A-Z]\\S+ |[y&IV0-9]+ ){1,5}(?=(OSWF|(W|w)indfarm))",n=4) %>% 
  data.frame(stringsAsFactors=F) %>% setNames(c("z","n")) -> df
df$a = str_remove_all(df$z, "The |MW |GW | [0-9]") %>% trimws
farms = group_by(df, a) %>% summarise(freq=sum(n)) %>% arrange(a) %>% data.frame
subset(farms, freq>=20) %>% arrange(a) %>% 
  write.csv("data/tmp.txt", quote=F, row.names=F)

extr("(\\S+ ){4}\\(BOWL\\)")
extr("Beatrice Offshore Wind ?(F|f)arm L(td|imited)|Beatrice|\\bBOWL\\b")
extr("Beatrice( Offshore)? Wind ?(F|f)arm( L(td|imited))?|Beatrice|\\bBOWL\\b")
extr("Beatrice(?!= Offshore Wind ?(F|f)arm)")
str_subset(S$tx, "GE Haliade")
extr("Pelamis( Wave Power)?")


###### Abbr. ###################################################
extr("\\b[A-Z\\-]{2,6}\\b",n=10)

extr("DNV( |-)GL")
extr("(\\S+ ){2}\\(GL\\)")
extr("E\\.ON")
extr("(\\S+ ){3}\\(RWE\\)")
extr("GE Renewable Energy|(G|g)eneral (E|e)lectric|\\bGE\\b")
extr("MHI( |-)Vestas|\\bMHI\\b")
extr("DONG Energy Wind Power|Dong Energy|\\b(.rsted|DONG|Dong)\\b")
extr("Siemens Gamesa( Renewable Energy)?|Siemens Wind Power( Service)?|Siemens(?!=( AG| Industry| Financial))|\\bSGRE\\b")
extr("Siemens (AG|Industry|Financial Services)")
extr("(\\w+ ){2}Energy Project", n=3)

subset(n.phrase, ngram == 3) %>% head(20)


str_subset(S$tx, "EPCI") %>% tail(10)
str_subset(S$tx, "\\((TenneT)\\)") %>% tail(10)

extr("Van oord",T)
extr("(A|a)utonomous (U|u)nderwater (V|v)ehicle|\\bAUV\\b")
extr("Royal Boskalis Westminister( N\\.V| NV)?")
extr("Board of Public Utilities|\\bBPU\\b",T)

# >= 15

###### Abbr. ###################################################
extr("\\b[A-Z]{2,6}\\b",n=20)

str_subset(S$tx, " PPE ") %>% tail(10)
str_subset(S$tx, "\\(BMWi\\)") %>% tail(10)

extr("Federal Ministry (of|for) Economics?( Affairs)?( and Energy)?|\\bBMWi\\b")
extr("\\bPPE\\b")


df_x <- data.frame("month" = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"),
                   "year" = c(2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020))


df_y <- data.frame("month" = c("Jan","Feb","Apr","May","Jul","Aug","Oct", "Nov"),
                   "year" =  c(2020,2020,2020,2020,2020,2020,2020,2020))


not_merge <- left_join(df_x,df_y, by = "month")
