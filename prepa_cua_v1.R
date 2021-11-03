
  
# library
library(knitr)
library(tidyverse)
library(lubridate)
library(rio)
library(readxl)
library(ggrepel) # pour les labels des plots
library(limer)

library(ggdist)
library(gghalves)    
library(rcartocolor) 


source(file="~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RFct/sumScoreDf.fct.R")
source(file="~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RFct/meanScoreDf.fct.R")
source(file="~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RFct/convScoreDf.fct.R")
source(file="~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RFct/alphaDf.fct.R")
source("~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RFct/corTestDf.fct.R")
source("~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RFct/formCor.fct.R")

load(file="~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RShare/cuaParam.df")
load(file="~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RShare/cecParam.df")
load(file="~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RShare/tsesParam.df")
load(file="~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RShare/capParam.df")
load(file="~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RShare/tesParam.df")
load(file="~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RShare/metParam.df")
load(file="~/OneDrive - HEP Vaud/1_Recherche_HEPL/WoRk/R/Work/Lib/RShare/tipParam.df")

library(corrplot)
library(psych)
library(ggplot2)

# load data from limesurvey
options(lime_api = "https://sondage.competences-emotionnelles.ch/admin/remotecontrol")
options(lime_username = "nbr_low")
options(lime_password = "82BBdJyTjqzz")

# log in
get_session_key()

# data
#responses <- get_responses("198632", sResponseType = "short")  
responses <- get_responses("725354", sResponseType = "short")  
d <- responses



#log out
release_session_key()



# le travail se fait à partir de d



# préparation des données et des scores

# supprimer les "points" et renommer les variables
d <- d %>% 
  rename_with(~ gsub('[[:punct:]]$', '', .x)) %>% 
  rename_with(~ gsub('[[:punct:]]', '_', .x)) %>%
  select(!c("lastpage","seed","startdate","submitdate",)) %>% 
  rename(lan = startlanguage, dat = datestamp)

# supprimer les 2 premiers participants
d <- d[-c(1:2),]

#info
info.df <- d[1:11]
info.df$id <- as.factor(info.df$id)
info.df$lan <- as.factor(info.df$lan)
info.df$dat <- as.factor(info.df$dat)
info.df$pro <- as.factor(info.df$pro)
info.df$sex <- as.factor(info.df$sex)
info.df$can <- as.factor(info.df$can)
info.df$sec <- as.factor(info.df$sec)
summary(info.df)

#cua
cua.df <- d[11:30]
min(cua.df, na.rm=T); max(cua.df, na.rm=T)
convScoreDf.fct(cua.df, cuaParam.df$cuaParamG, 1, 4) -> cuaQConv.df
sumScoreDf.fct(cuaQConv.df, cuaParam.df) -> cuaVD.df
meanScoreDf.fct(cuaQConv.df, cuaParam.df) -> cuaVD.df
summary(cua.df)


#cec (empathie)
cec.df <- d[31:48]
min(cec.df, na.rm=T); max(cec.df, na.rm=T)
convScoreDf.fct(cec.df, cecParam.df$cecParamG, 1, 5) -> cecQConv.df
sumScoreDf.fct(cecQConv.df, cecParam.df) -> cecVD.df
meanScoreDf.fct(cecQConv.df, cecParam.df) -> cecVD.df
summary(cec.df)

#tses
tses.df <- d[49:60]
min(tses.df, na.rm=T); max(tses.df, na.rm=T)
convScoreDf.fct(tses.df, tsesParam.df$tsesParamG, 1, 9) -> tsesQConv.df
sumScoreDf.fct(tsesQConv.df, tsesParam.df) -> tsesVD.df
meanScoreDf.fct(tsesQConv.df, tsesParam.df) -> tsesVD.df
summary(tses.df)


#cap
cap.df <- d[61:70]
min(cap.df, na.rm=T); max(cap.df, na.rm=T)
convScoreDf.fct(cap.df, capParam.df$capParamG, 1, 6) -> capQConv.df
sumScoreDf.fct(capQConv.df, capParam.df) -> capVD.df
meanScoreDf.fct(capQConv.df, capParam.df) -> capVD.df
summary(cap.df)


#tes
tes.df <- d[71:82]
min(tes.df, na.rm=T); max(tes.df, na.rm=T)
convScoreDf.fct(tes.df, tesParam.df$tesParamG, 1, 4) -> tesQConv.df
sumScoreDf.fct(tesQConv.df, tesParam.df) -> tesVD.df
meanScoreDf.fct(tesQConv.df, tesParam.df) -> tesVD.df
summary(tes.df)


#met
met.df <- d[83:92]
min(met.df, na.rm=T); max(met.df, na.rm=T)
convScoreDf.fct(met.df, metParam.df$metParamG, 1, 5) -> metQConv.df
sumScoreDf.fct(metQConv.df, metParam.df) -> metVD.df
meanScoreDf.fct(metQConv.df, metParam.df) -> metVD.df
summary(met.df)


#tip
tip.df <- d[93:102]
min(tip.df, na.rm=T); max(tip.df, na.rm=T)
convScoreDf.fct(tip.df, tipParam.df$tipParamG, 1, 7) -> tipQConv.df
sumScoreDf.fct(tipQConv.df, tipParam.df) -> tipVD.df
meanScoreDf.fct(tipQConv.df, tipParam.df) -> tipVD.df
summary(tip.df)





# combine VD and describe

allVD.df <- cbind(cuaVD.df[-1], cecVD.df[-1], tsesVD.df[-1], capVD.df[-1], tesVD.df[-1], metVD.df) #, tipVD.df)
allVD.df <- cbind(info.df, tsesVD.df, capVD.df, tesVD.df, metVD.df)
names(allVD.df)
summary(allVD.df)
describe(allVD.df[11:24])



# corrélations validité concourante CUA
#allVD.df <- cbind(cuaVD.df, capVD.df, metVD.df)
corTestDf.fct(allVD.df, allVD.df) -> tmp.li
formCor.fct(tmp.li)

M<-cor(allVD.df)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200), tl.cex=.5, number.cex=0.5,
         type="upper", #order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         ## Combiner avec le niveau de significativité
         #p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)


# corrélations validité prédictive CUA
#allVD.df <- cbind(cuaVD.df, tesVD.df, tsesVD.df, cecVD.df, tipVD.df)
#corTestDf.fct(allVD.df, allVD.df) -> tmp.li
#formCor.fct(tmp.li)




# alpha: KO pour            cec, cap (SIC+INA), metParamPROC, tipi
round(alphaDf.fct(cov(cuaQConv.df, use="pairwise"), cuaParam.df), 2)
round(alphaDf.fct(cov(cecQConv.df, use="pairwise"), cecParam.df), 2)
round(alphaDf.fct(cov(tsesQConv.df, use="pairwise"), tsesParam.df), 2)
round(alphaDf.fct(cov(capQConv.df, use="pairwise"), capParam.df), 2)
round(alphaDf.fct(cov(tesQConv.df, use="pairwise"), tesParam.df), 2)
round(alphaDf.fct(cov(metQConv.df, use="pairwise"), metParam.df), 2)
round(alphaDf.fct(cov(tipQConv.df, use="pairwise"), tipParam.df), 2)

corTestDf.fct(tipQConv.df, tipQConv.df) -> tmp.li
formCor.fct(tmp.li)
corTestDf.fct(tip.df, tip.df) -> tmp.li
formCor.fct(tmp.li)



summary(allVD.df)
#describe(allVD.df[11:24])







### PLOT
boxplot(cuaVD.df[-1], notch=T, main="Scores aux dimensions de la CUA")
violin(cuaVD.df[-1], main="Niveaux de CUA", las=2)

boxplot(cecVD.df[-1], notch=T, main="Niveaux de contagion, empathie et coupure")
violin(cecVD.df[-1], main="Niveaux de contagion, empathie et coupure", col=c("coral2", "lightgreen", "grey"))

boxplot(tsesVD.df[-1], notch=T, main="Efficacité perçue pour engager des élèves (ESE), mettre en \n place des stratégies d'apprentissage (EIS) et gérer la classe (ECM)")
violin(tsesVD.df[-1], main="Efficacité perçue (pour engager, stratégie et GC)", col=c("red", "blue", "green"))

violin(capVD.df[-1], main="Croyances sur l'apprentissage comme simple (vs. complexe), \n inné (vs. acquis) et convergent (vs. divergent)", col=c("red", "blue", "green"))

violin(tesVD.df[-1], main="Emotions dans l'enseignement", col=c("yellow", "red", "purple"))

violin(metVD.df, main="Représentations de l’enseignement orientées \n vers les processus et vers les contenus")

violin(tipVD.df[-1], main="Personnalité", las=2)


###########################################################################################  
###########################################################################################  
### more elaborated
###########################################################################################  
###########################################################################################  

## colour theme
my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2)]
## theme for horizontal charts
theme_flip <-
  theme(
    axis.text.x = element_text(face = "plain", family = "Roboto Mono", size = 22),
    axis.text.y = element_text(face = "bold", family = "Roboto", size = 26),
    panel.grid.major.x = element_line(color = "grey90", size = .6),
    panel.grid.major.y = element_blank(),
    legend.position = "top", 
    legend.text = element_text(family = "Roboto Mono", size = 18),
    legend.title = element_text(face = "bold", size = 18, margin = margin(b = 25)))



###########################################################################################  
### more elaborated

# cua => scores de 1à4 sur 5 items pas échelle
colnames(cuaVD.df) <- c("G", "Engager", "Représenter", "Exprimer", "Enseigner")
mf <- stack(cuaVD.df[-1])
ggplot(mf, aes(x = ind, y = values, fill=ind)) +     
  geom_boxplot(notch = TRUE, alpha=0.3)+
  geom_jitter(aes(color = ind), width=0.15, alpha = 0.6) +
  theme(legend.position = "none")+
  labs(x = "",  y = "Moyenne",title ="Diversification...", subtitle = "...",caption = "Résultats sur 100 enseignants",alt = "Add alt text to the plot")

t <-  ggplot(mf, aes(x = forcats::fct_rev(ind), y = values, color = ind, fill = ind)) +
  geom_boxplot(width = .2, fill = "white", size = 0.5, outlier.shape = NA, notch = T) +
  ggdist::stat_halfeye(adjust = .33, width = .67, color = NA, position = position_nudge(x = .15)) +
  gghalves::geom_half_point(side = "l",range_scale = .3, alpha = .5, size = .8)   +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07)) +
  scale_y_continuous(breaks = 1:9) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")   +
  theme_light()

t+  labs(x = "",  y = "", title ="Diversification...", subtitle = "", caption = "N=109", alt = "Add alt text to the plot")


###########################################################################################  
# cec => scores de 1à5 sur 6 items pas échelle
colnames(cecVD.df) <- c("G", "Contagion", "Empathie", "Coupure")
mf <- stack(cecVD.df[-1])
ggplot(mf, aes(x = ind, y = values, fill=ind)) +     
  geom_boxplot(notch = TRUE, alpha=0.3)+
  geom_jitter(aes(color = ind), width=0.15, alpha = 0.6) +
  theme(legend.position = "none")+
  labs(x = "",  y = "Moyenne",title ="Ecoute...", subtitle = "...contagion, empathie et coupure",caption = "Résultats sur 100 enseignants",alt = "Add alt text to the plot")

t <-  ggplot(mf, aes(x = forcats::fct_rev(ind), y = values, color = ind, fill = ind)) +
  geom_boxplot(width = .2, fill = "white", size = 0.5, outlier.shape = NA, notch = T) +
  ggdist::stat_halfeye(adjust = .33, width = .67, color = NA, position = position_nudge(x = .15)) +
  gghalves::geom_half_point(side = "l",range_scale = .3, alpha = .5, size = .8)   +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07)) +
  scale_y_continuous(breaks = 1:9) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")   +
  theme_light()

t+  labs(x = "",  y = "", title ="Ecoute...", subtitle = "", caption = "N=109", alt = "Add alt text to the plot")


###########################################################################################  
# t-se-s => scores de 1à9 sur 4 items pas échelle
colnames(tsesVD.df) <- c("G", "Engagement", "Stratégies", "Gestion de classe")
mf <- stack(tsesVD.df[-1])
ggplot(mf, aes(x = ind, y = values, fill=ind)) +     
  geom_boxplot(notch = TRUE, alpha=0.3)+
  geom_jitter(aes(color = ind), width=0.15, alpha = 0.6) +
  theme(legend.position = "none")+
  labs(x = "",  y = "Moyenne",title ="Efficacité perçue pour....", subtitle = "...engager, stratégies et gérer la classe",caption = "Résultats sur 100 enseignants",alt = "Add alt text to the plot")

  ggplot(mf, aes(x = ind, y = values, fill=ind)) +
    geom_violin(aes(fill=ind), alpha = 0.2)+
    geom_jitter(aes(color = ind), width=0.15, alpha = 0.6) +
    theme(legend.position = "none")

t <-  ggplot(mf, aes(x = forcats::fct_rev(ind), y = values, color = ind, fill = ind)) +
    geom_boxplot(width = .2, fill = "white", size = 0.5, outlier.shape = NA, notch = T) +
    ggdist::stat_halfeye(adjust = .33, width = .67, color = NA, position = position_nudge(x = .15)) +
    gghalves::geom_half_point(side = "l",range_scale = .3, alpha = .5, size = .8)   +
    coord_flip() +
    scale_x_discrete(expand = c(.07, .07)) +
    scale_y_continuous(breaks = 1:9) +
    scale_color_manual(values = my_pal, guide = "none") +
    scale_fill_manual(values = my_pal, guide = "none")   +
    theme_light()

t+  labs(x = "",  y = "", title ="Efficacité perçue", subtitle = "", caption = "N=109", alt = "Add alt text to the plot")
  #theme_linedraw()
  #theme_flip
  
  
  
###########################################################################################  
# cap => scores de 1à6 sur 3-3-4 items par dim
colnames(capVD.df) <- c("G", "Simple-complexe", "Inné-acquis", "Convergent-divergent")
boxplot(capVD.df[-1], notch=T, main="Croyances sur l'apprentissage comme simple (vs. complexe), \n inné (vs. acquis) et convergent (vs. divergent)")
mf <- stack(capVD.df[-1])
ggplot(mf, aes(x = ind, y = values, fill=ind)) +     
  geom_boxplot(notch = TRUE, alpha=0.3)+
  geom_jitter(aes(color = ind), width=0.15, alpha = 0.6) +
  theme(legend.position = "none")+
  labs(x = "",  y = "Moyenne",title ="Croyances sur l'apprentissage", subtitle = "simple (/comp), inné (/acquis) et convergent (vs. div)",caption = "Résultats sur 100 enseignants",alt = "Add alt text to the plot")



t <-  ggplot(mf, aes(x = forcats::fct_rev(ind), y = values, color = ind, fill = ind)) +
  geom_boxplot(width = .2, fill = "white", size = 0.5, outlier.shape = NA, notch = T) +
  ggdist::stat_halfeye(adjust = .33, width = .67, color = NA, position = position_nudge(x = .15)) +
  gghalves::geom_half_point(side = "l",range_scale = .3, alpha = .5, size = .8)   +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07)) +
  scale_y_continuous(breaks = 1:9) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")   +
  theme_light()

t+  labs(x = "",  y = "", title ="Croyances sur l'apprentissage", subtitle = "", caption = "N=109", alt = "Add alt text to the plot")


###########################################################################################  
# tes => scores de 1à4 sur 4 items pas échelle
colnames(tesVD.df) <- c("G", "Joie", "Colère", "Anxiété")
boxplot(tesVD.df[-1], notch=T, main="Emotions dans l'enseignement")
mf <- stack(tesVD.df[-1])
ggplot(mf, aes(x = ind, y = values, fill=ind)) +     
  geom_boxplot(notch = TRUE, alpha=0.3)+
  geom_jitter(aes(color = ind), width=0.15, alpha = 0.6) +
  theme(legend.position = "none")+
  labs(x = "",  y = "Moyenne",title ="Emotions dans l'enseignement", subtitle = "",caption = "Résultats sur 100 enseignants",alt = "Add alt text to the plot")


t <-  ggplot(mf, aes(x = forcats::fct_rev(ind), y = values, color = ind, fill = ind)) +
  geom_boxplot(width = .2, fill = "white", size = 0.5, outlier.shape = NA, notch = T) +
  ggdist::stat_halfeye(adjust = .33, width = .67, color = NA, position = position_nudge(x = .15)) +
  gghalves::geom_half_point(side = "l",range_scale = .3, alpha = .5, size = .8)   +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07)) +
  scale_y_continuous(breaks = 1:9) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")   +
  theme_light()

t+  labs(x = "",  y = "", title ="Emotions dans l'enseignement", subtitle = "", caption = "N=109", alt = "Add alt text to the plot")


###########################################################################################  
# met => scores de 1à5 sur 5 items pas échelle
colnames(metVD.df) <- c("Orienté processus", "Orienté contenu")
boxplot(metVD.df, notch=T, main="Représentations de l’enseignement orientées \n vers les processus et vers les contenus")
mf <- stack(metVD.df)
ggplot(mf, aes(x = ind, y = values, fill=ind)) +     
  geom_boxplot(notch = TRUE, alpha=0.3)+
  geom_jitter(aes(color = ind), width=0.15, alpha = 0.6) +
  theme(legend.position = "none")+
  labs(x = "",  y = "Moyenne",title ="Représentations de l’enseignement", subtitle = "orientées processus et contenus",caption = "Résultats sur 100 enseignants",alt = "Add alt text to the plot")

t <-  ggplot(mf, aes(x = forcats::fct_rev(ind), y = values, color = ind, fill = ind)) +
  geom_boxplot(width = .2, fill = "white", size = 0.5, outlier.shape = NA, notch = T) +
  ggdist::stat_halfeye(adjust = .33, width = .67, color = NA, position = position_nudge(x = .15)) +
  gghalves::geom_half_point(side = "l",range_scale = .3, alpha = .5, size = .8)   +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07)) +
  scale_y_continuous(breaks = 1:9) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")   +
  theme_light()

t+  labs(x = "",  y = "", title ="Représentations de l’enseignement", subtitle = "", caption = "N=109", alt = "Add alt text to the plot")



###########################################################################################  
# tipi scores de 1à7 avec 2 items par échelle
colnames(tipVD.df) <- c("G", "Extraversion", "Agressivité", "Conscience", "Stabilité émotionnelle", "Ouverture")
boxplot(tipVD.df, notch=T, main="Facettes de personnalité O.C.E.A.N.")
mf <- stack(tipVD.df[-1])
ggplot(mf, aes(x = ind, y = values, fill=ind)) +     
  geom_boxplot(notch = TRUE, alpha=0.3)+
  geom_jitter(aes(color = ind), width=0.15, alpha = 0.6) +
  theme(legend.position = "none")+
  labs(x = "",  y = "Moyenne",title ="Facettes de personnalité", subtitle = "O.C.E.A.N.",caption = "Résultats sur 100 enseignants",alt = "Add alt text to the plot")

t <-  ggplot(mf, aes(x = forcats::fct_rev(ind), y = values, color = ind, fill = ind)) +
  geom_boxplot(width = .2, fill = "white", size = 0.5, outlier.shape = NA, notch = T) +
  ggdist::stat_halfeye(adjust = .33, width = .67, color = NA, position = position_nudge(x = .15)) +
  gghalves::geom_half_point(side = "l",range_scale = .3, alpha = .5, size = .8)   +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07)) +
  scale_y_continuous(breaks = 1:9) +
 # scale_color_manual(values = my_pal, guide = "none") +
#  scale_fill_manual(values = my_pal, guide = "none")   +
  theme_light()

t+  labs(x = "",  y = "", title ="Représentations de l’enseignement", subtitle = "", caption = "N=109", alt = "Add alt text to the plot")



