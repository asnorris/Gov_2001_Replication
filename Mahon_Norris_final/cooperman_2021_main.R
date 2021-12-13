####################################
# This is code to replicate the analyses and figures from the main article
# (Un)Natural Disasters: Electoral Cycles in Disaster Relief
# Code developed by Alicia Cooperman
####################################
# Analysis conducted using
# R version 4.0.2 (2020-06-22) 
# on Platform: x86_64-apple-darwin17.0 (64-bit)
####################################

rm(list=ls(all=TRUE))
dev.off()

library(lfe) # Version 2.8.5
library(stargazer) # Version 5.2.2

load("cooperman_2021_data.Rdata") 
source("cooperman_2021_functions.R") 

# Figure 2: Rainfall Deviation and Drought Emergency Declaration -------------------------------------------------------

smoothing.pdf <- .3
bin.size.pdf <- .15
pdf(file = "fig2.pdf", onefile=FALSE,paper="special",width=15,height=20)
par(mfrow=c(5,2))
create.rd.plot.pdf(z=df$SPI_6_June,y = df$drought_bin,  bin.size=bin.size.pdf, smoothing=smoothing.pdf, titles = "ALL STATES (1031)")
statelabels <- c("AL", "BA", "CE", "MG", "PB", "PE", "PI", "RN", "SE")
statenames <- c("Alagoas (35)", "Bahia (257)", "Ceará (134)", "Minas Gerais (40)", "Paraíba (170)","Pernambuco (118)", "Piauí (109)", "Rio Grande do Norte (140)", "Sergipe (28)")
for (S in 1:length(statelabels)){
create.rd.plot.pdf(bin.size=bin.size.pdf,smoothing=smoothing.pdf, z=df$SPI_6_June[df$state==statelabels[S] & !is.na(df$drought_bin)],y = df$drought_bin[df$state==statelabels[S] & !is.na(df$drought_bin)], state=statelabels[S], titles=statenames[S])
}
dev.off()

# Table 1: Election Years and Drought Declaration ----------------------------------------------------------------

elec.noint <- felm(drought_bin ~ belowave6mo_June + mayorelecyear  + govelecyear + time + time.sq  + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df)
elec.may <- felm(drought_bin ~ belowave6mo_June*mayorelecyear + time  + time.sq+ PT + PET_JantoJune +  lncattle + corn_share + bean_share  |ibgecode|0|state.year, data=df)
elec.maygov <- felm(drought_bin ~ belowave6mo_June*mayorelecyear  + govelecyear*belowave6mo_June + time + time.sq + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df)

mayorgov_int_labels <- c("Below Average Rainfall", "Mayor Election Year",  "State/Fed Election Year", "Mayor Election * Below Ave. Rainfall", "State/Fed Election * Below Ave. Rainfall")
cat(stargazer(elec.noint,elec.may, elec.maygov, omit=c("time", "PT", "PET_JantoJun", "lncattle", "corn_share", "bean_share"),  dep.var.labels="Drought Emergency Declared",
              covariate.labels=mayorgov_int_labels,float=FALSE,omit.stat=c("ll", "f"))
    , file="tab1.tex", sep="\n")


# Table 2: Mayoral Term and Drought Declaration ---------------------------

lameduck.below0 <- felm(drought_bin ~ lameduck + mayorelecyear + time + time.sq + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode |0|state.year, data=df, subset =SPI_6_June<0)
lameduck.below0.int <- felm(drought_bin ~ lameduck*mayorelecyear + time + time.sq + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode |0|state.year, data=df, subset =SPI_6_June<0)
lameduck.above0 <- felm(drought_bin ~ lameduck + mayorelecyear + time + time.sq + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df, subset =SPI_6_June>0)
lameduck.above0.int <- felm(drought_bin ~ lameduck*mayorelecyear + time + time.sq + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df, subset =SPI_6_June>0)
lameduck.above1 <- felm(drought_bin ~ lameduck + mayorelecyear + time + time.sq + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df, subset =SPI_6_June>1)
lameduck.above1.int <- felm(drought_bin ~ lameduck*mayorelecyear + time + time.sq + PT  + PET_JantoJune +  lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df, subset =SPI_6_June>1)

cat(stargazer(lameduck.below0,lameduck.below0.int, lameduck.above0, lameduck.above0.int, lameduck.above1,lameduck.above1.int,
              omit=c("PT", "PET_JantoJune", "lncattle", "corn_share", "bean_share", "time"),
              float=FALSE, dep.var.labels="Drought Emergency Declared",
              covariate.labels = c("Second Term Mayor", "Mayor Election Year", "Mayor Election * Second Term Mayor"), 
              column.labels = c("Below Ave.", "Below Ave.", "Above Ave.","Above Ave.", "Very High", "Very High"), df = F)
    , file="tab2.tex", sep="\n")


# Table 3: Incumbent Mayor Candidacy and Re-Election ----------------------

incrun0 <- felm(incrun~ belowave6mo_June + PET_JantoJune + mayor_win_vshare_prev + copartisan_pres + copartisan_gov + lncattle + corn_share + bean_share |state + year|0|state.year, data=df, subset=lameduck==0)
incrun1 <- felm(incrun~ drought_mayor + mayor_win_vshare_prev   + copartisan_pres + copartisan_gov   + lncattle + corn_share + bean_share |state + year|0|state.year, data=df, subset=lameduck==0)
incrun3 <- felm(incrun~ drought_mayor_2years + mayor_win_vshare_prev + copartisan_pres + copartisan_gov    + lncattle + corn_share + bean_share |state + year|0|state.year, data=df, subset=lameduck==0)
incwin0 <- felm(incwin~ belowave6mo_June + PET_JantoJune + mayor_win_vshare_prev  + copartisan_pres + copartisan_gov   + lncattle + corn_share + bean_share |state + year|0|state.year, data=df, subset=lameduck==0)
incwin1 <- felm(incwin~ drought_mayor + mayor_win_vshare_prev + copartisan_pres + copartisan_gov   + lncattle + corn_share + bean_share |state + year|0|state.year, data=df, subset=lameduck==0)
incwin3 <- felm(incwin~ drought_mayor_2years + mayor_win_vshare_prev + copartisan_pres + copartisan_gov   + lncattle + corn_share + bean_share |state + year|0|state.year, data=df, subset=lameduck==0)
incrunwinlabels <- c("Below Average Rainfall", "Drought Declaration in Mayor Election Year","Drought Declaration in Last Two Years", "Mayor's Vote Share in Previous Election", "Copartisan President", "Copartisan Governor")
cat(stargazer(incrun0, incrun1,incrun3,
              incwin0, incwin1,incwin3,
              omit=c("PET_JantoJune", "lncattle", "corn_share", "bean_share"),covariate.labels=incrunwinlabels,
              float=FALSE, dep.var.labels=c("Incumbent Runs","Incumbent Wins"),df = F)
    , file="tab3.tex", sep="\n")


# Figure 3: Incumbent Mayor Wins Re-Election by Drought -------------------

pdf(file = "fig3.pdf", onefile=FALSE,paper="special",width=10,height=7)
d <- subset(df, lameduck==0)
d <- d[with(d, order(SPI_6_June)), ]
smoothing.pdf <- .8
bin.size.pdf <- .25
plot(d$SPI_6_June, d$incwin, col="white", xlim=c(-2.5,2.5), ylim=c(0,1),xlab="SPI (Jan-June)", ylab="Incumbent Wins", cex.lab=1.5, cex.axis=1.5)
create.rd.lines.incwin(z=d$SPI_6_June[d$drought_mayor==0],y = d$incwin[d$drought_mayor==0], bin.size=bin.size.pdf, smoothing=smoothing.pdf, lty=2)
create.rd.lines.incwin(z=d$SPI_6_June[d$drought_mayor==1],y = d$incwin[d$drought_mayor==1], bin.size=bin.size.pdf, smoothing=smoothing.pdf, lty=1)
legend("bottomright", legend=c("Declaration", "No Declaration"), lty=c(1,2), cex=1.5)
dev.off()


# Table 4: Partisan Cycles and Drought Declaration ------------------------------------------------------

pt.clusterstate <-       felm(drought_bin ~ mayorelecyear*PT + SPI_6_June + PET_JantoJune + time + time.sq + lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df, subset=PT_variation==1)
copartpres.clusterstate <- felm(drought_bin ~ mayorelecyear*copartisan_pres + SPI_6_June + PET_JantoJune + time + time.sq +lncattle + corn_share + bean_share  |ibgecode|0|state.year, data=df, subset= copartisan_pres_variation==1)
copartgov.clusterstate <- felm(drought_bin ~ mayorelecyear*copartisan_gov + SPI_6_June + PET_JantoJune + time + time.sq +lncattle + corn_share + bean_share   |ibgecode|0|state.year, data=df, subset=copartisan_gov_variation==1)
prescoal.clusterstate <- felm(drought_bin ~ mayorelecyear*pres_coalition_mayor + SPI_6_June + PET_JantoJune + time + time.sq +lncattle + corn_share + bean_share |ibgecode|0|state.year, data=df, subset= pres_coalition_mayor_variation==1)
alignmentlabels <- c("Mayor Election Year","PT", "Copartisan Mayor President", "Copartisan Mayor Governor", "Copartisan Mayor President Coalition", "Mayor Elec. * PT", "Mayor Elec. * Co. Mayor Pres.", "Mayor Elect. * Co. Mayor Gov.","Mayor Elec. * Co. Mayor Pres. Coal.")
df2 <- subset(df,!is.na(lncattle))
df2 <- subset(df2,!is.na(corn_share))
df2 <- subset(df2,!is.na(bean_share))

cat(stargazer(pt.clusterstate,copartpres.clusterstate,copartgov.clusterstate,prescoal.clusterstate
              ,omit=c("SPI", "PET_JantoJune", "lncattle", "corn_share", "bean_share", "time")
              ,add.lines=list(c("Number of Municipalities", length(unique(df2$ibgecode[df2$PT_variation==1])),  length(unique(df2$ibgecode[df2$copartisan_pres_variation==1])), length(unique(df2$ibgecode[df2$copartisan_gov_variation==1])), length(unique(df2$ibgecode[df2$pres_coalition_mayor_variation==1]))))
              , dep.var.labels="Drought Emergency Declared",float=FALSE, covariate.labels=alignmentlabels
              , omit.stat=c("ll", "f", "ser"), no.space=F), file="tab4.tex", sep="\n")


