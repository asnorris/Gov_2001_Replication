library(tidyverse)
library(lfe) # Version 2.8.5
library(stargazer) # Version 5.2.2

# read in data
load("Mahon_Norris_final/cooperman_2021_data.Rdata") 
# source("cooperman_2021_functions.R") 

# create a folder for exhibits to be added to
dir.create("Mahon_Norris_final/Exhibits")

##################
## FIGURE 1
##################

fig1 <- df %>%
  mutate(lameduck = as.factor(lameduck)) %>%
   ggplot(aes(x = SPI_6_June, y = drought_bin, group = lameduck, color = lameduck)) +
  geom_smooth() +
  labs(x = "SPI (Jan-June)", y = "Drought Declaration", color = "") +
  scale_color_discrete(labels = c("Not Term Limited", "Term Limited")) 

# save figure to exhibit folder
ggsave(path = "Mahon_Norris_final/Exhibits",
       device = "png", filename = "figure1.pdf", plot = fig1, width = 7, height = 4)


##################
## TABLE 1
##################

# edit data so that there are categorical "below average variables for 
df2 <- df %>%
  mutate(belowave3mo = ifelse(SPI_3_June > 0,0,1)) %>%
  mutate(belowave9mo = ifelse(SPI_9_June > 0,0,1)) %>%
  mutate(recent_drought = ifelse(belowave9mo > belowave6mo_June, 1,0)) 

# original model 3 from table 1
elec.noint <- felm(drought_bin ~ belowave6mo_June + mayorelecyear  + govelecyear + time + time.sq  + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df)

# three month data
elec.noint2 <- felm(drought_bin ~ belowave3mo + mayorelecyear  + govelecyear + time + time.sq  + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df2)

# nine month data
elec.noint3 <- felm(drought_bin ~ belowave9mo + mayorelecyear  + govelecyear + time + time.sq  + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df2)

# three months before election
elec.noint4 <- felm(drought_bin ~ recent_drought + mayorelecyear  + govelecyear + time + time.sq  + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df2)

mayorgov_int_labels <- c("Below Average Rainfall (Jan-June)", "Below Average Rainfall (Apr-June)", "Below Average Rainfall (Jan-Sept)", "Below Average Rainfall (June-Sept)", "Mayor Election Year",  "State/Fed Election Year")

cat(stargazer(elec.noint, elec.noint2, elec.noint3, elec.noint4, omit=c("time", "PT", "PET_JantoJun", "lncattle", "corn_share", "bean_share"),  dep.var.labels="Drought Emergency Declared",
              covariate.labels=mayorgov_int_labels,float=FALSE,omit.stat=c("ll", "f")), file="Mahon_Norris_final/Exhibits/tab1.tex", sep="\n")



##################
## TABLE 2
##################

# incumbent running models
incrun0 <- felm(incrun~ belowave6mo_June + PET_JantoJune + mayor_win_vshare_prev + copartisan_pres + copartisan_gov + lncattle + corn_share + bean_share |state + year|0|state.year, data=df, subset=lameduck==0)
incrun01 <- felm(incrun~ SPI_6_June + PET_JantoJune + mayor_win_vshare_prev + copartisan_pres + copartisan_gov + lncattle + corn_share + bean_share |state + year|0|state.year, data=df, subset=lameduck==0)

# incumbent winning models
incwin0 <- felm(incwin~ belowave6mo_June + PET_JantoJune + mayor_win_vshare_prev  + copartisan_pres + copartisan_gov   + lncattle + corn_share + bean_share |state + year|0|state.year, data=df, subset=lameduck==0)
incwin01 <- felm(incwin~ SPI_6_June + PET_JantoJune + mayor_win_vshare_prev  + copartisan_pres + copartisan_gov   + lncattle + corn_share + bean_share |state + year|0|state.year, data=df, subset=lameduck==0)

incrunwinlabels <- c("Rainfall Compared to Average (effect of more rainfall)", "Below Average Rainfall",
                     "Mayor's Vote Share in Previous Election", "Copartisan President", "Copartisan Governor")

cat(stargazer(incrun01, incrun0,
              incwin01, incwin0,
              omit=c("PET_JantoJune", "lncattle", "corn_share", "bean_share"),covariate.labels=incrunwinlabels,
              float=FALSE, dep.var.labels=c("Incumbent Runs","Incumbent Wins"),df = F)
    , file="Mahon_Norris_final/Exhibits/tab2.tex", sep="\n")


