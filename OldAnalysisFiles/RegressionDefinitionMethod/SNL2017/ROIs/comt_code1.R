setwd('~/Dropbox/Documents/Haskins/comt/comt_stats_tables_5.3')

library(phia)

behav = read.table('COMT_data.txt', sep='\t', header=T)
volume = read.table('aparc.volume.stats.txt', sep='\t', header=T)
thickness = read.table('aparc.thickness.stats.txt', sep='\t', header=T)
area = read.table('aparc.area.stats.txt', sep='\t', header=T)
subcortical = read.table('aseg.volume.stats.dat', sep='\t', header=T)

fsvolume = merge(behav, volume, by='Subject')
fsarea = merge(behav, area, by='Subject')
fsthickness = merge(behav, thickness, by='Subject')
fssubcortical = merge(behav, subcortical, by='Subject')

#get ICV
#check group assignments
table(fsvolume$Group)

#calculate mean thickness
fsthickness$MeanThickness = apply(fsthickness[c('lh_MeanThickness_thickness', 'rh_MeanThickness_thickness')], 1, mean)
fsthickness$MeanThickness = fsthickness$MeanThickness - mean(fsthickness$MeanThickness, na.rm=T)

#demean ICV to make life easier
fssubcortical$ICV = fssubcortical$EstimatedTotalIntraCranialVol - mean(fssubcortical$EstimatedTotalIntraCranialVol, na.rm=T)

#run models for volume
volumeVars = names(fsvolume[19:86])
fsvolume$ICV = fssubcortical$ICV

sink('volume.txt')

models = lapply( volumeVars, function(x) { lm(substitute( i ~ Group * ICV, list(i=as.name(x)) ), data=fsvolume ) } )
lapply(models, function(y) { print(anova(y)); print(testInteractions(y, adjustment='none'))})

sink()

##########################FIGURES VOLUME##############################
library(ggplot2)

pdf('volume.pdf')
for (i in volumeVars) {
	boxplot( fsvolume[,i] ~ Group, data=fsvolume, na.action=na.omit, main=i )	
}
dev.off()

######################################################################

thicknessVars = names(fsthickness[19:87])

sink('thickness.txt')

models = lapply( thicknessVars, function(x) { lm(substitute( i ~ Group * MeanThickness, list(i=as.name(x)) ), data=fsthickness ) } )
lapply(models, function(y) { print(anova(y)); print(testInteractions(y, adjustment='none'))})

sink()

##########################FIGURES THICKNESS##############################
library(ggplot2)

pdf('thickness.pdf')
for (i in thicknessVars) {
	boxplot( fsthickness[,i] ~ Group, data= fsthickness, na.action=na.omit, main=i )	
}
dev.off()

######################################################################


areaVars = names(fsarea[19:88])
fsarea$ICV = fssubcortical$ICV

sink('area.txt')

models = lapply( areaVars, function(x) { lm(substitute( i ~ Group * ICV, list(i=as.name(x)) ), data= fsarea ) } )
lapply(models, function(y) { print(anova(y)); print(testInteractions(y, adjustment='none'))})

sink()

##########################FIGURES AREA##############################
library(ggplot2)

pdf('area.pdf')
for (i in areaVars) {
	boxplot( fsarea[,i] ~ Group, data= fsarea, na.action=na.omit, main=i )	
}
dev.off()

######################################################################

subcorticalVars = names(fssubcortical[c(19:51, 59:74)])

sink('subcortical.txt')

models = lapply( subcorticalVars, function(x) { lm(substitute( i ~ Group * ICV, list(i=as.name(x)) ), data= fssubcortical ) } )
lapply(models, function(y) { print(anova(y)); print(testInteractions(y, adjustment='none'))})

sink()

##########################FIGURES SUBCORTICAL##############################
library(ggplot2)

pdf('subcortical.pdf')
for (i in subcorticalVars) {
	boxplot( fssubcortical[,i] ~ Group, data= fssubcortical, na.action=na.omit, main=i )	
}
dev.off()

######################################################################

######################################## Complex Models
#word attack, spelling and PA

volumeVars = names(fsvolume[19:86])
fsvolume$ICV = fssubcortical$ICV

sink('volume_WA.txt')

models = lapply( volumeVars, function(x) { lm(substitute( i ~ Group * SS_WordAttack * ICV, list(i=as.name(x)) ), data=fsvolume ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='SS_WordAttack', adjustment='none'))})

sink()

########################## FIGURES COMPLEX 1 ##############################
library(ggplot2)

pdf('volume_WA_plots.pdf')
for (i in volumeVars) {
	print(ggplot(fsvolume, aes(x=fsvolume[,i], y=SS_WordAttack)) + geom_point() + stat_smooth(method=lm) + theme_bw() + xlab(paste(i, "Volume")))
}
dev.off()

######################################################################

sink('volume_spell.txt')

models = lapply( volumeVars, function(x) { lm(substitute( i ~ Group * SS_Spelling * ICV, list(i=as.name(x)) ), data=fsvolume ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='SS_Spelling', adjustment='none'))})

sink()

sink('volume_PA.txt')

models = lapply( volumeVars, function(x) { lm(substitute( i ~ Group * CTOPP_PhonoAwareness * ICV, list(i=as.name(x)) ), data=fsvolume ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='CTOPP_PhonoAwareness', adjustment='none'))})

sink()

########################## FIGURES COMPLEX 2 ##############################
library(ggplot2)

pdf('volume_PA_plots.pdf')
for (i in volumeVars) {
	print(ggplot(fsvolume, aes(x=fsvolume[,i], y= CTOPP_PhonoAwareness)) + geom_point() + stat_smooth(method=lm) + theme_bw() + xlab(paste(i, "Volume")))
}
dev.off()

######################################################################

thicknessVars = names(fsthickness[19:87])

sink('thickness_WA.txt')

models = lapply( thicknessVars, function(x) { lm(substitute( i ~ Group * SS_WordAttack * MeanThickness, list(i=as.name(x)) ), data=fsthickness ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='SS_WordAttack', adjustment='none'))})

sink()

########################## FIGURES COMPLEX 2 ##############################
library(ggplot2)

pdf('thickness_WA_plots.pdf')
for (i in thicknessVars) {
	print(ggplot(fsthickness, aes(x= fsthickness[,i], y= SS_WordAttack)) + geom_point() + stat_smooth(method=lm) + theme_bw() + xlab(paste(i, "Thickness")))
}
dev.off()

######################################################################

sink('thickness_spell.txt')

models = lapply( thicknessVars, function(x) { lm(substitute( i ~ Group * SS_Spelling * MeanThickness, list(i=as.name(x)) ), data=fsthickness ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='SS_Spelling', adjustment='none'))})

sink()

sink('thickness_PA.txt')

models = lapply( thicknessVars, function(x) { lm(substitute( i ~ Group * CTOPP_PhonoAwareness * MeanThickness, list(i=as.name(x)) ), data=fsthickness ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='CTOPP_PhonoAwareness', adjustment='none'))})

sink()

########################## FIGURES COMPLEX 2 ##############################
library(ggplot2)

pdf('thickness_PA_plots.pdf')
for (i in thicknessVars) {
	print(ggplot(fsthickness, aes(x= fsthickness[,i], y= CTOPP_PhonoAwareness)) + geom_point() + stat_smooth(method=lm) + theme_bw() + xlab(paste(i, "Thickness")))
}
dev.off()

######################################################################

areaVars = names(fsarea[19:88])
fsarea$ICV = fssubcortical$ICV

sink('area_WA.txt')

models = lapply( areaVars, function(x) { lm(substitute( i ~ Group * SS_WordAttack * ICV, list(i=as.name(x)) ), data= fsarea ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='SS_WordAttack', adjustment='none'))})

sink()

sink('area_spell.txt')

models = lapply( areaVars, function(x) { lm(substitute( i ~ Group * SS_Spelling * ICV, list(i=as.name(x)) ), data= fsarea ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='SS_Spelling', adjustment='none'))})

sink()

sink('area_PA.txt')

models = lapply( areaVars, function(x) { lm(substitute( i ~ Group * CTOPP_PhonoAwareness * ICV, list(i=as.name(x)) ), data= fsarea ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='CTOPP_PhonoAwareness', adjustment='none'))})

sink()

subcorticalVars = names(fssubcortical[c(19:51, 59:74)])

sink('subcortical_WA.txt')

models = lapply( subcorticalVars, function(x) { lm(substitute( i ~ Group * SS_WordAttack * ICV, list(i=as.name(x)) ), data= fssubcortical ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='SS_WordAttack', adjustment='none'))})

sink()

########################## FIGURES COMPLEX 2 ##############################
library(ggplot2)

pdf('subcortical_WA_plots.pdf')
for (i in subcorticalVars) {
	print(ggplot(fssubcortical, aes(x= fssubcortical[,i], y= SS_WordAttack)) + geom_point() + stat_smooth(method=lm) + theme_bw() + xlab(paste(i, "Thickness")))
}
dev.off()

######################################################################

sink('subcortical_spell.txt')

models = lapply( subcorticalVars, function(x) { lm(substitute( i ~ Group * SS_Spelling * ICV, list(i=as.name(x)) ), data= fssubcortical ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='SS_Spelling', adjustment='none'))})

sink()

sink('subcortical_PA.txt')

models = lapply( subcorticalVars, function(x) { lm(substitute( i ~ Group * CTOPP_PhonoAwareness * ICV, list(i=as.name(x)) ), data= fssubcortical ) } )
sapply(models, function(y) { print(anova(y)); print(testInteractions(y, slope='CTOPP_PhonoAwareness', adjustment='none'))})

sink()

########################## FIGURES COMPLEX 2 ##############################
library(ggplot2)

pdf('subcortical_PA_plots.pdf')
for (i in subcorticalVars) {
	print(ggplot(fssubcortical, aes(x= fssubcortical[,i], y= CTOPP_PhonoAwareness)) + geom_point() + stat_smooth(method=lm) + theme_bw() + xlab(paste(i, "Thickness")))
}
dev.off()

######################################################################

######################################### Contrasts

library(phia)

sink('contrasts.txt')

working=c(1:33, 36, 40:53)
lapply(models[working], function(y) { testInteractions(y, pairwise='Group', slope='IntraCranialVol', adjustment ='none') } )

sink()

#testing code
library(phia)


test = lm( Right.Caudate ~ -1 + Group * IntraCranialVol, data=final )
summary(test)
anova(test)

tapply(final$Right.Caudate, final$Group, mean)
interactionMeans(test)
testInteractions(test, pairwise='Group', slope='IntraCranialVol', adjustment="none")

model.matrix(test)