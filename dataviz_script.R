# ===== chargement, isntallation des libraries ====
require(rjson)
require(jsonlite)
require(ggplot2)
require(ggplotgui)
require(forcats)
require(RColorBrewer)
require(stringr)
require(RColorBrewer)
require(viridis)
require(gridExtra)
require(gghighlight)
require(dplyr)
require(lubridate)

# ==== telechargement du dataset en licence ouverte ====
# https://stackoverflow.com/questions/2617600/importing-data-from-a-json-file-into-r
# json_file <- "http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"
json_file <- "https://www.data.gouv.fr/fr/datasets/r/16962018-5c31-4296-9454-5998585496d2"
# on convertir en flatten
json_data <- jsonlite::fromJSON(json_file, flatten=TRUE)
# head(json_data)
# View(json_data)

# on check la class
class(json_data)

# on convertit en df
df0 <- as.data.frame(json_data)
class(df0)

# dimension du tableur (nombre de lignes, nombre de colonnes)
dim(df0)[1]

names(df0)

# on teste quelques valeurs uniques
unique(df0$marches.source)
unique(df0$marches.nature)
unique(df0$marches.codeCPV)
unique(df0$marches.lieuExecution.code)
unique(df0$marches.lieuExecution.typeCode)
unique(df0$marches.dureeMois)
as.data.frame(table(df0$marches.dureeMois))
unique(df0$marches.formePrix)
length(unique(df0$marches.codeCPV)) # 2906 Code CPV differents impossible a representer en un graphe

# ==== 00 creation de dataframe temporaire un peu plus propre ====
df1 <- subset(df0, !is.na(marches.nature))
df2 <- subset(df1, !is.na(df1$marches.nature))
df3 <- subset(df2, marches.montant <=500000 & marches.nature!="Marché de partenariat")
df3 <- subset(df2, marches.montant<=10000000 & marches.montant>100)

df3$date2 <- strptime(as.character(df3$marches.dateNotification), "%Y-%m-%d")
df3$date2 <- as.POSIXct(df3$date2)
df3$date2 <- as.Date(df3$date2)
df3$date_fin <- df3$date2 + ddays(df3$marches.dureeMois*30) 

df4 <- subset(df3, !is.na(marches.datePublicationDonnees))
df4$date2 <- as.POSIXlt(df4$date2)
df4$date2 <- strptime(as.character(df4$date_pub), "%Y-%m-%d")
# str(df4)
df4$date2 <- as.POSIXct(df4$date2)
df4$date3 <- as.Date(df4$date2)
df4$date_fin <- df4$date2 + ddays(df4$marches.dureeMois*30) 
df_2019 <- subset(df4, year(df4$date2)=='2019')


# ===== 00: dataviz part 1 =====
# ===== 01 : fig 1 graphes de valeurs uniques en une page   =====
p1 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(marches.source)))) + coord_flip() + theme_bw() + labs(y="nombre", x="source")
p2 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(marches.formePrix)))) + coord_flip() + theme_bw() + labs(y="nombre", x="nature")
p3 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(marches._type)))) + coord_flip() + theme_bw() + labs(x="nombre", x="type")
# p4 <- ggplot(df0) + geom_bar(aes(x=fct_infreq(marches.codeCPV))) + coord_flip()
p4 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(marches.lieuExecution.typeCode))))+ coord_flip() + theme_bw() + labs(y="nombre", x="type de lieu \ d'execution")
p5 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(str_wrap(marches.nature,30))))) + coord_flip() + theme_bw() + labs(y="nombre", x="nature")
p6 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(str_wrap(marches.procedure,40))))) + coord_flip()  + theme_bw() + labs(y="nombre", x="procédure")
p7 <- ggplot(df0) + geom_histogram(aes(x=marches.dureeMois))  + theme_bw() +  labs(y="nombre", x="durée en Mois")
p8 <- ggplot(df0) + geom_histogram(aes(x=marches.montant)) +scale_x_log10(
  labels = function(x) format(x, scientific = FALSE)) + theme_bw() + labs(y="nombre", x="Montant")

# on regroupe le tout sur une seule page 
ggpubr::ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,  legend="none") 

# ==== 02 : fig 2 graphes de valeurs uniques en une page en couleur la source ====
c1 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(marches.source)), fill=marches.source)) + coord_flip() + theme_bw() + labs(y="nombre", x="source")
c2 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(marches.formePrix)),fill=marches.source)) + coord_flip() + theme_bw() + labs(y="nombre", x="nature")
c3 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(marches._type)), fill=marches.source)) + coord_flip() + theme_bw() + labs(x="nombre", x="type")
# p4 <- ggplot(df0) + geom_bar(aes(x=fct_infreq(marches.codeCPV))) + coord_flip()
c4 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(marches.lieuExecution.typeCode)),fill=marches.source))+ coord_flip() + theme_bw() + labs(y="nombre", x="type de lieu \ d'execution")
c5 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(str_wrap(marches.nature,30))),fill=marches.source)) + coord_flip() + theme_bw() + labs(y="nombre", x="nature")
c6 <- ggplot(df0) + geom_bar(aes(x=fct_rev(fct_infreq(str_wrap(marches.procedure,40))),fill=marches.source)) + coord_flip()  + theme_bw() + labs(y="nombre", x="procédure")
c7 <- ggplot(subset(df0, marches.dureeMois<6*12)) + geom_histogram(aes(x=marches.dureeMois,fill=marches.source),binwidth = 6, color="gray20") +
  scale_x_continuous(breaks = c(seq(0,6*12,6)))   + theme_bw() +  labs(y="nombre", x="durée en Mois")
p8 <- ggplot(df0) + geom_histogram(aes(x=marches.montant, fill= marches.source), color="gray20") +scale_x_log10(
  labels = function(x) format(x, scientific = FALSE)) + theme_bw() + labs(y="nombre", x="Montant")

# on regroupe le tout sur une seule page 
ggpubr::ggarrange(c1,c2,c3,c4,c5,c6,c7,c8,  common.legend = TRUE, legend="top") 

p1 <- ggplot(df1) + geom_bar(aes(x=fct_rev(fct_infreq(marches.source)), fill=marches.source)) + coord_flip() + theme_bw() + labs(y="nombre", x="source")
p2 <- ggplot(df1) + geom_bar(aes(x=fct_rev(fct_infreq(marches.formePrix)),fill=marches.source)) + coord_flip() + theme_bw() + labs(y="nombre", x="nature")
p3 <- ggplot(df1) + geom_bar(aes(x=fct_rev(fct_infreq(marches._type)), fill=marches.source)) + coord_flip() + theme_bw() + labs(x="nombre", x="type")
# p4 <- ggplot(df1) + geom_bar(aes(x=fct_infreq(marches.codeCPV))) + coord_flip()
p4 <- ggplot(df1) + geom_bar(aes(x=fct_rev(fct_infreq(marches.lieuExecution.typeCode)),fill=marches.source))+ coord_flip() + theme_bw() + labs(y="nombre", x="type de lieu \ d'execution")
p5 <- ggplot(df1) + geom_bar(aes(x=fct_rev(fct_infreq(str_wrap(marches.nature,30))),fill=marches.source)) + coord_flip() + theme_bw() + labs(y="nombre", x="nature")
p6 <- ggplot(df1) + geom_bar(aes(x=fct_rev(fct_infreq(str_wrap(marches.procedure,40))),fill=marches.source)) + coord_flip()  + theme_bw() + labs(y="nombre", x="procédure")
p7 <- ggplot(subset(df1, marches.dureeMois<6*12)) + geom_histogram(aes(x=marches.dureeMois,fill=marches.source),binwidth = 6, color="gray20") +
  scale_x_continuous(breaks = c(seq(0,6*12,6)))   + theme_bw() +  labs(y="nombre", x="durée en Mois")
p8 <- ggplot(df1) + geom_histogram(aes(x=marches.montant, fill= marches.source), color="gray20") +scale_x_log10(
  labels = function(x) format(x, scientific = FALSE)) + theme_bw() + labs(y="nombre", x="Montant")

ggpubr::ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,  common.legend = TRUE, legend="top") 


# ==== fig 4 : Dispersion des durées selon nature des marchés ====
fig4 <- ggplot(subset(df2, marches.dureeMois<(6*12))) + 
  geom_boxplot(aes(y=marches.dureeMois, x=marches.nature, fill=marches.nature)) +
  scale_y_continuous(breaks = c(seq(0,6*12,6)))   + theme_bw() +  labs(x="Nature", y="durée en Mois") +
  scale_fill_brewer(palette = "Spectral") + theme(legend.position = "bottom")

# ==== fig 5 : graphe en "Violon" entre la durée en Mois et la nature des Marchés ====
fig5 <- ggplot(subset(df2, marches.dureeMois<(6*12))) + 
  geom_violin(aes(y=marches.dureeMois, x=marches.nature, fill=marches.nature)) +
  scale_y_continuous(breaks = c(seq(0,6*12,6)))   + theme_bw() +  labs(x="nature", y="durée en Mois") +
  scale_fill_brewer(palette = "Spectral") + theme(legend.position = "bottom")

# ==== fig 6 : Histogramme des marchés selon leur nature
# https://ggplot2.tidyverse.org/reference/theme.html
ggplot(df3) + 
  geom_histogram(aes(fill=marches.nature, x=marches.montant/1000), position="stack", binwidth = 25, color="gray20") +
  scale_x_continuous(breaks = c(seq(0,500,25)))  +  labs(x="Montant en K€", y="Nombre de marchés") +
  scale_fill_brewer(palette = "Spectral") + theme_bw()  + theme(
    legend.position = c(0.75,0.75), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill="gray95",
                                     size=0.5, linetype="solid", 
                                     colour ="gray30")
  )

# ==== fig 7 : histogramme avec gghighlight =====
ggplot(df3) + 
  geom_histogram(aes(fill=marches.nature, x=marches.montant/1000), position="stack", binwidth = 25, color="gray20") +
  scale_x_continuous(breaks = c(seq(0,500,25)))  +  labs(x="Montant en K€", y="Nombre de marchés") +
  scale_fill_brewer(palette = "Spectral") + theme_bw()  + theme(
    legend.position = c(0.75,0.75), 
    axis.text.x = element_text(size=7),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill="gray95",
                                     size=0.5, linetype="solid", 
                                     colour ="gray30")
  )  +  gghighlight() +  facet_wrap(marches.source~.) 


# ===== 01: dataviz part 2 =====
# nettoyage de data selon temps
# === on zappe les record sans date  
df3 <- subset(df3, !is.na(marches.datePublicationDonnees))
# === on zappe les iso long ====
df3$marches.dateNotification<- gsub("+01:00", '', df3$marches.dateNotification)
df3$marches.dateNotification<- gsub('+02:00', '', df3$marches.dateNotification)
df3$marches.dateNotification<- gsub('+03:00', '', df3$marches.dateNotification)
df3$marches.dateNotification<- gsub("+", '', df3$marches.dateNotification)


# ==== fig 8 : Date de notification et durée ====

ggplot(df4) + geom_point(aes(x=round_date(date2, "week"), y=marches.montant, color=marches.nature))+ 
  geom_segment(aes(x=round_date(date2, "week"), xend=date_fin, y=marches.montant, yend=marches.montant, color=marches.nature)) +
  theme_bw() + facet_grid(marches.source~.) + theme(legend.position = "bottom") + labs(x="Date de notification", y="Montant des marchés") +
  scale_color_brewer(palette = "Spectral")

# ==== fig 9 subset pour une seule source ====
ggplot(subset(df4, marches.source=="marches-publics.info")) + geom_point(aes(x=round_date(date2, "week"), y=marches.montant, color=marches.nature))+ 
  geom_segment(aes(x=round_date(date2, "week"), xend=date_fin, y=marches.montant, yend=marches.montant, color=marches.nature, alpha=0.45)) +
  theme_bw()  + theme(legend.position = "bottom") + labs(x="Date de notification", y="Montant des marchés") +
  scale_color_brewer(palette = "Spectral")
