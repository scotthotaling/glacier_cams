# Install relevant packages
# Remove "#" to make the commands available
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("rstatix")
# install.packages("forcats")
# install.packages("gridExtra")
# install.packages("zoo")
# install.packages("ggforce")

# Load ggplot2
library(ggplot2)
library(scales)
library(rstatix)
library(forcats)
library(gridExtra)
library(zoo)
library(ggforce)
library(dplyr)

# Read in the data
setwd('<path to your working directory')
MORA_data <- read.csv("MORA_results_nov21_forR.csv")
MORA_corr_all <- read.csv("MORA_correlations_all.csv")

### Combining all cameras on a week-by-week basis

# Check for normality in the data
shapiro.test(MORA_corr_all$Human)
shapiro.test(MORA_corr_all$Non_human)

# Perform an overall correlation for the data set
cor.test(MORA_corr_all$Human, MORA_corr_all$Non_human, method="spearman")


###/// Observations >> Humans by Camera (barchart) ///###

# Make six filtered data sets (one per camera)

MORA_data_c1 <- filter(MORA_data, Site == "C1")
MORA_data_c2 <- filter(MORA_data, Site == "C2")
MORA_data_c3 <- filter(MORA_data, Site == "C3")
MORA_data_c4 <- filter(MORA_data, Site == "C4")
MORA_data_c5 <- filter(MORA_data, Site == "C5")
MORA_data_c6 <- filter(MORA_data, Site == "C6")


# Making plots

humans_c1 <- ggplot()+
	geom_col(data=MORA_data_c1, aes(x=Order, y=Human), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,45)
	
humans_c1

humans_c2 <- ggplot()+
	geom_col(data=MORA_data_c2, aes(x=Order, y=Human), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,45)
	
humans_c2

humans_c3 <- ggplot()+
	geom_col(data=MORA_data_c3, aes(x=Order, y=Human), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,45)
	
humans_c3

humans_c4 <- ggplot()+
	geom_col(data=MORA_data_c4, aes(x=Order, y=Human), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,45)
	
humans_c4

humans_c5 <- ggplot()+
	geom_col(data=MORA_data_c5, aes(x=Order, y=Human), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,45)
	
humans_c5

humans_c6 <- ggplot()+
	geom_col(data=MORA_data_c6, aes(x=Order, y=Human), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,45)
	
humans_c6


###/// Observations >> Non-carnivore by Camera (barchart) ///###

non_carn_c1 <- ggplot()+
	geom_col(data=MORA_data_c1, aes(x=Order, y=Non_carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
non_carn_c1

non_carn_c2 <- ggplot()+
	geom_col(data=MORA_data_c2, aes(x=Order, y=Non_carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
non_carn_c2

non_carn_c3 <- ggplot()+
	geom_col(data=MORA_data_c3, aes(x=Order, y=Non_carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
non_carn_c3

non_carn_c4 <- ggplot()+
	geom_col(data=MORA_data_c4, aes(x=Order, y=Non_carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
non_carn_c4

non_carn_c5 <- ggplot()+
	geom_col(data=MORA_data_c5, aes(x=Order, y=Non_carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
non_carn_c5

non_carn_c6 <- ggplot()+
	geom_col(data=MORA_data_c6, aes(x=Order, y=Non_carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
non_carn_c6


###/// Observations >> Carnivore by Camera (barchart) ///###

carn_c1 <- ggplot()+
	geom_col(data=MORA_data_c1, aes(x=Order, y=Carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
carn_c1

carn_c2 <- ggplot()+
	geom_col(data=MORA_data_c2, aes(x=Order, y=Carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
carn_c2

carn_c3 <- ggplot()+
	geom_col(data=MORA_data_c3, aes(x=Order, y=Carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
carn_c3

carn_c4 <- ggplot()+
	geom_col(data=MORA_data_c4, aes(x=Order, y=Carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
carn_c4

carn_c5 <- ggplot()+
	geom_col(data=MORA_data_c5, aes(x=Order, y=Carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
carn_c5

carn_c6 <- ggplot()+
	geom_col(data=MORA_data_c6, aes(x=Order, y=Carnivore), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
carn_c6


###/// Observations >> Birds by Camera (barchart) ///###

birds_c1 <- ggplot()+
	geom_col(data=MORA_data_c1, aes(x=Order, y=All_birds), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
birds_c1

birds_c2 <- ggplot()+
	geom_col(data=MORA_data_c2, aes(x=Order, y=All_birds), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
birds_c2

birds_c3 <- ggplot()+
	geom_col(data=MORA_data_c3, aes(x=Order, y=All_birds), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
birds_c3

birds_c4 <- ggplot()+
	geom_col(data=MORA_data_c4, aes(x=Order, y=All_birds), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
birds_c4

birds_c5 <- ggplot()+
	geom_col(data=MORA_data_c5, aes(x=Order, y=All_birds), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
birds_c5

birds_c6 <- ggplot()+
	geom_col(data=MORA_data_c6, aes(x=Order, y=All_birds), fill = "darkslategray")+
	theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
	ylim(0,5)
	
birds_c6


##### Put plots together #####

humans_all <- grid.arrange(humans_c1, humans_c2, humans_c3, humans_c4, humans_c5, humans_c6, ncol = 1, nrow = 6)
non_carn_all <- grid.arrange(non_carn_c1, non_carn_c2, non_carn_c3, non_carn_c4, non_carn_c5, non_carn_c6, ncol = 1, nrow = 6)
carn_all <- grid.arrange(carn_c1, carn_c2, carn_c3, carn_c4, carn_c5, carn_c6, ncol = 1, nrow = 6)
birds_all <- grid.arrange(birds_c1, birds_c2, birds_c3, birds_c4, birds_c5, birds_c6, ncol = 1, nrow = 6)

all_cams <- grid.arrange(humans_all, non_carn_all, carn_all, birds_all, ncol = 4, nrow = 1)























