################################################### Set WD , Load Packages & Attach Dataset ##############################################
#########################################################################################################################################

# setup
	#setwd("G:\\Politikwissenschaft\\Team-Bailer\\Digital Lifes\\r-scripts\\r-scripts survey")
	#setwd("C:\\Users\\suttad00\\Basel Powi Dropbox\\Adrian Sutter\\DigitalLives\\r-scripts\\r-scripts survey")
	setwd("C:\\Users\\turnerzw\\Basel Powi Dropbox\\Tomas Zwinkels\\DigitalLives\\r-scripts\\r-scripts survey")


	#install.packages("dplyr")
	#install.packages("readxl")
	#install.packages("ggplot2")
	#install.packages("sqldf")
	#install.packages("XLConnect")
	#install.packages("stargazer")
	install.packages("lme4")
	#install.packages("merTools")
	#install.packages("afex")

	library(readxl)
	library(dplyr)
	library(ggplot2)
	library(sqldf)
	library(XLConnect)
	library(lme4)
	library(car)
	library(viridis)
#	library(merTools)
#	library(lmerTest)
#	library(afex)

	#attach(PilotV1_DO_clean)
	#attach(PilotV1_DO_clean_tomastries)

	# loading the data from the surveys 

	PilotV2_clean_CH_DE <- as.data.frame(read_excel("PilotV2_for_R_Clean.xlsx"))
	PilotV2_clean_CH_FR <- as.data.frame(read_excel("PilotV2_for_R_Clean_CH_FR_TLA.xlsx"))
	PilotV2_clean_DE <- as.data.frame(read_excel("PilotV2_for_R_Clean_DE_TLA.xlsx"))

	# bind the excel sheets together

	DF <- bind_rows(PilotV2_clean_CH_DE,PilotV2_clean_CH_FR,PilotV2_clean_DE)
	head(DF)
	summary(DF)
	colnames(DF)

	# select key variables of interest
		varstotake <- c("Age","Gender","Gender_politician","voting_kanton","Education","SocialMediaUse","political_content","Follow_Politician", 
					"SocialMediaPosting","party_treatment","left_right_scale_1","presented_party","suitability","L2V","Warmth"
					,"Credibility","Thermometer","NameTreatment", "country","Party","Party2ndChoice","language")

		AnalysisDF <- DF[varstotake]
		head(AnalysisDF)
		summary(AnalysisDF)

	# get dummies that indicate thar respodents did not have a party preference or a preference for an obscure party
		
		# this should be given by the 'Party' dummy, lets first inspect its completeness accross the three data-frames
		table(AnalysisDF$Party)
		table(is.na(AnalysisDF$Party))

		# no party preference dummy
		
			# first party
				AnalysisDF$nofirstpartypref <- ifelse((AnalysisDF$Party == "Ich weiss es nicht"| AnalysisDF$Party == "Je ne sais pas"),"no pref","has pref") 
				# should this not be 'no first party preference' and 'no second'? --o
				# is a little complicated because of this matters also depends on what you where shown!
				
				# distribution accross countries 
				table(AnalysisDF$nofirstpartypref)
				table(AnalysisDF$country)
				table(AnalysisDF$nofirstpartypref,AnalysisDF$country)
				prop.table(table(AnalysisDF$nofirstpartypref,AnalysisDF$country),2) # quite a bit less germans (15% versus 25% in CH, with no party preference... is this representative of the population?!)
		
			# second party preference
				AnalysisDF$nosecondpartypref <- ifelse((AnalysisDF$Party2ndChoice == "Ich weiss es nicht"| AnalysisDF$Party2ndChoice == "Je ne sais pas"),"no pref","has pref") 
				table(AnalysisDF$nosecondpartypref) # interesting, people seem to find this easier?!
				
			# no preference for selected condition, captures is somebody was randomly selected into the condition where one of the two parties above was shown but did not have a pref (should be in the case in about 1/3 of these cases?)
			
				AnalysisDF$prefincondition <- ifelse(
													(AnalysisDF$presented_party == "Own Party" & AnalysisDF$nofirstpartypref == "no pref")
													|
													(AnalysisDF$presented_party == "2nd Party" & AnalysisDF$nosecondpartypref == "no pref")
													,"no pref in selected condition","had pref in selected condition")
			
		# obscure party dummy
			AnalysisDF$obscurepartydummy <- ifelse((AnalysisDF$Party == "Partei:"| 
													AnalysisDF$Party == "Parti:" | 
													AnalysisDF$Party2ndChoice == "Partei:" | 
													AnalysisDF$Party2ndChoice == "Parti:"),
													"obscure","regular") 
			table(AnalysisDF$obscurepartydummy)
			table(AnalysisDF$obscurepartydummy,AnalysisDF$country)
			prop.table(table(AnalysisDF$obscurepartydummy,AnalysisDF$country),2) # slightly more 'obscure' parties in DE
			
		# no party shown dummy
			table(AnalysisDF$presented_party)
			summary(AnalysisDF$presented_party)
			AnalysisDF$nopartytreatment <- ifelse(AnalysisDF$presented_party == "No Party","none shown","a party was shown") 
			table(AnalysisDF$nopartytreatment)
			
			table(AnalysisDF$presented_party,AnalysisDF$language)
		
	# get the perceived left/right party positions in that Natalie provided


		

		# 'party_treatment' contains the parties that respondents saw <- I think with the fixes I implemented this is now indeed true
		table(AnalysisDF$party_treatment) 
		table(AnalysisDF$party_treatment,AnalysisDF$country)
		table(AnalysisDF$party_treatment,AnalysisDF$language) 
		# how can this be 'I don't know?!, also contains values like 'Partei:' and 'Parti:' so that can also not be correct? 
		# --> the answer seems to be that this is 'correct'; we randomly select one of three following conditions:
			# 1. party of first choice
			# 2. party of second choice
			# 3. party of third choice
		# if however you did not have a 'second choice' party or mentioned a weird party then you can end up seing(?!) I don't know' here? <- this probalby means we should just exclude these people?!
		
			
		# import the file
			PAPO = read.csv("partypositionsDE_CH_withmatchinglabels.csv", header = TRUE, sep = ";")
			summary(PAPO)
			head(PAPO)
			names(PAPO)
			
			head(AnalysisDF)
			
		# merge
			AnalysisDF$party_treatment <- as.character(AnalysisDF$party_treatment)
			
			AnalysisDF$country <- as.character(AnalysisDF$country)
			
			PAPO$avg_perc_lmer <- as.numeric(as.character(PAPO$average_perceived_left_right_position))
			PAPO$abv_in_survey <- as.character(PAPO$abv_in_survey)
			PAPO$country <- as.character(PAPO$country)
			
			TEMP <- sqldf("
						   SELECT AnalysisDF.*, PAPO.party_parlgov_id, PAPO.avg_perc_lmer
						   FROM AnalysisDF LEFT JOIN PAPO
						   ON
							 (
  							  AnalysisDF.party_treatment = PAPO.abv_in_survey
							   AND 
							  AnalysisDF.country = PAPO.country
							  )
						   
						  ")
			nrow(AnalysisDF)
			nrow(TEMP)
			head(TEMP)
			AnalysisDF <- TEMP
			
			# inspect the results
				summary(AnalysisDF$avg_perc_lmer)
				table(is.na(AnalysisDF$avg_perc_lmer))
			
			# proper 'remaining' missing cases
				AnalysisDF$othercases <- ifelse(AnalysisDF$obscurepartydummy == "obscure" | 
												AnalysisDF$prefincondition == "no pref in selected condition" | 
												AnalysisDF$nopartytreatment == "none shown",
												"known case","other case")
				table(AnalysisDF$othercases)
			
												
				table(is.na(AnalysisDF$avg_perc_lmer),AnalysisDF$othercases)	# 619 cases left -- parties missing?

				REMCAS <- AnalysisDF[which(is.na(AnalysisDF$avg_perc_lmer) & AnalysisDF$othercases == "other case"),]
				nrow(REMCAS)
				
				table(REMCAS$party_treatment)
				table(REMCAS$party_treatment,REMCAS$language)
		
		# now calculcate the distance between the own left/right position and the one of the presented party
		
			# inspect the left/right scales
				summary(AnalysisDF$left_right_scale_1)
				hist(AnalysisDF$left_right_scale_1,breaks=20)
				hist(AnalysisDF$avg_perc_lmer,breaks=20)
			
			# lets transform our one to a scale 0 to 10
				AnalysisDF$left_right_scale_010 <- AnalysisDF$left_right_scale_1/10
				summary(AnalysisDF$left_right_scale_010)	
			
			# bunch of checks that should show results
				
				# first, lets get some stuff in
					TEMP2 <- sqldf("
							   SELECT AnalysisDF.*, PAPO.avg_perc_lmer 'lmer_of_first_party'
							   FROM AnalysisDF LEFT JOIN PAPO
							   ON
								 (
								  AnalysisDF.Party = PAPO.abv_in_survey
								   AND 
								  AnalysisDF.country = PAPO.country
								  )
							   
							  ")
					nrow(AnalysisDF)
					nrow(TEMP2)
					head(TEMP2)
					AnalysisDF <- TEMP2
				
				# this is really just a check if merging was done correctly etc
				
					# select the cases that got to see their own prefence
					DFSFP <- AnalysisDF[which(AnalysisDF$party_treatment == AnalysisDF$Party),]
					table(DFSFP$presented_party) # looks good, some people had same second party as first party?
					AnalysisDF[which(AnalysisDF$Party2ndChoice == AnalysisDF$Party),] # these are all people that wrote down custom party choices in both first and second choice
					
			
					# use this selection of all cases that saw their first choice party, and see if correlation between own left/right position and the one of the party is indeed strong
					plot(DFSFP$avg_perc_lmer,DFSFP$left_right_scale_010)
					cor(DFSFP$avg_perc_lmer,DFSFP$left_right_scale_010,use="pairwise.complete.obs")
					
					# and for the second party choice
					DFSSP <- AnalysisDF[which(AnalysisDF$party_treatment == AnalysisDF$Party2ndChoice),]
					nrow(DFSSP)
					plot(DFSSP$avg_perc_lmer,DFSSP$left_right_scale_010)
					cor(DFSSP$avg_perc_lmer,DFSSP$left_right_scale_010,use="pairwise.complete.obs") # indeed weaker
					
					# other party (should be a negative correlation?)
					table(AnalysisDF$presented_party)
					DFOTH <- AnalysisDF[which(AnalysisDF$presented_party == "Other Party"),]
					nrow(DFOTH)
					plot(DFOTH$avg_perc_lmer,DFOTH$left_right_scale_010)
					cor(DFOTH$avg_perc_lmer,DFOTH$left_right_scale_010,use="pairwise.complete.obs") # indeed weaker
					
					# no party - should flatline -- ow yes, ofcourse.. then we do not have any party info!
					DFNO <- AnalysisDF[which(AnalysisDF$presented_party == "No Party"),]
					nrow(DFNO)
					plot(DFNO$avg_perc_lmer,DFNO$left_right_scale_010)
					cor(DFNO$avg_perc_lmer,DFNO$left_right_scale_010,use="pairwise.complete.obs") # indeed weaker
					
					# in one graphic
					ggplot(AnalysisDF, aes(x=avg_perc_lmer, y=left_right_scale_010, color=presented_party, shape=presented_party)) +
					  geom_point() + 
					  geom_smooth(method=lm, aes(fill=presented_party)) +
					  ylab("respondents self-reported left right score") +
					  xlab("average perceived left right score for party in selects2015 and DEU2017")
			
			# lets calculcate the distance measure
			
				AnalysisDF$abslmerdif <- abs(AnalysisDF$left_right_scale_010 - AnalysisDF$avg_perc_lmer)
				summary(AnalysisDF$abslmerdif)
				hist(AnalysisDF$abslmerdif) # looks like a nice distribution
				
			# and set this value to zero for all of the 'known cases'
				AnalysisDF$abslmerdifformodel <- ifelse(AnalysisDF$othercases == "known case",0,AnalysisDF$abslmerdif)
				summary(AnalysisDF$abslmerdifformodel) # only# 321 missing
				
			# list of variables to take towards the model
				#	abslmerdifformodel
				#	obscurepartydummy
				#	prefincondition
				#	nopartytreatment
				
###########################################################################################################################################
############################################################ DESCRIPTIVES #################################################################
##########################################################################################################################################


#write.csv(AnalysisDF, file = "AnalysisDF.csv",row.names=FALSE)

# lookup: doing descripiv tables in stargazer
stargazer(AnalysisDF,type="text")

# set NA values
AnalysisDF[AnalysisDF=="NA"] <- NA
AnalysisDF[AnalysisDF=="#VALUE!"] <- NA


#Some quick checks how the measurement-values of the treatments are distributed in the data, checking the measurement scale

# inspect the distribution of treatments 
AnalysisDF$NameTreatment <- as.factor(AnalysisDF$NameTreatment)
table(AnalysisDF$NameTreatment)

# set and inspect for the different measurements of measurement-scale
table(AnalysisDF$suitability)
AnalysisDF$suitability <- as.numeric(as.character(AnalysisDF$suitability))
table(AnalysisDF$suitability)
hist(AnalysisDF$suitability)

table(AnalysisDF$L2V)
AnalysisDF$L2V <- as.numeric(as.character(AnalysisDF$L2V))
table(AnalysisDF$L2V)
hist(AnalysisDF$L2V)

table(AnalysisDF$Warmth)
AnalysisDF$Warmth <- as.numeric(as.character(AnalysisDF$Warmth))
table(AnalysisDF$Warmth)
hist(AnalysisDF$Warmth)

table(AnalysisDF$Credibility)
AnalysisDF$Credibility <- as.numeric(as.character(AnalysisDF$Credibility))
table(AnalysisDF$Credibility)
hist(AnalysisDF$Credibility)

# make a scale out of the 4 main measurement items --> polscale = the mean of the 4 measurements, this will be used as dependent variable

AnalysisDF$polscale <- (AnalysisDF$suitability + AnalysisDF$L2V + AnalysisDF$Warmth + AnalysisDF$Credibility) / 4
hist(AnalysisDF$polscale)
summary(AnalysisDF$polscale)


# some additional descriptives

data.matrix(AnalysisDF, rownames.force = NA)
aggTest <- aggregate(AnalysisDF, by = list(AnalysisDF$NameTreatment), FUN = mean, simplify = T, drop = T)
aggTest


head(AnalysisDF)

data.frame(DF$Age, DF$Gender, DF$voting_kanton, DF$Education, DF$SocialMediaUse,
           DF$NameTreatment, DF$suitability, DF$L2V, DF$Warmth, DF$Credibility, DF$Thermometer)

head(AnalysisDF)

summary(AnalysisDF)



#######################################################################################################################################
################################################### !! DATA PREPARATION !! ############################################################
#######################################################################################################################################

library(plyr)
library(reshape2)

######  COMMENT:
######  to prepare the data for analysis new categories are introduced and some categories are collapsed into more generalizable categories


# make a combined category for left and right each; create DF's for left and right treatments

#left
table(AnalysisDF$NameTreatment)
AnalysisDF$Treatment_simple <- as.character(AnalysisDF$NameTreatment)
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_233_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_333_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_322_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_311-left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_233_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_333_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_322_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_133_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_123-left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_311-left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_133-left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_311_image-l")] <- "left_treatment"

AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_123_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_311_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_311_image_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_123_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_311_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_133-left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_133_left")] <- "left_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_311_image_left")] <- "left_treatment"

left_DF <- AnalysisDF[which(AnalysisDF$Treatment_simple == "left_treatment" & AnalysisDF$left_right_scale_1<=50),]


##########

#right
table(AnalysisDF$NameTreatment)
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_333_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_311_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_123_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_333_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_133_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_311-right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_233_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_123_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_322_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_133_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_322_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_233_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_311_image_r")] <- "right_treatment"

AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_123_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_311_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_311_image_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_123_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_311_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "female_311_image_right")] <- "right_treatment"
AnalysisDF$Treatment_simple[which(AnalysisDF$Treatment_simple == "male_133_right")] <- "right_treatment"

table(AnalysisDF$Treatment_simple)

right_DF <- AnalysisDF[which(AnalysisDF$Treatment_simple == "right_treatment" & AnalysisDF$left_right_scale_1>=50),]


#############################################################################################################
########################################## DUMMIES ##########################################################
#############################################################################################################


######    COMMENT: 
######    this section prepares several dummies which are the later used for analysis


#Adding Dummy to indicate if personal left-right orientation and left or right leaning treatment match or not

AnalysisDF$noleftrightmismatch <- ifelse(
                                (AnalysisDF$Treatment_simple == "left_treatment" & AnalysisDF$left_right_scale_1>=50) 
                                | 
                                (AnalysisDF$Treatment_simple == "right_treatment" & AnalysisDF$left_right_scale_1<=50),
                                "mismatch",
                                "no_mismatch")
table(AnalysisDF$noleftrightmismatch)

#Adding Dummy to indicate if personal left-right orientation and left or right oriented presented party match or not

AnalysisDF$nopartymismatch  <- as.character(AnalysisDF$presented_party)
                              AnalysisDF$nopartymismatch[which(AnalysisDF$nopartymismatch == "Other Party")] <- "mismatch"
                              AnalysisDF$nopartymismatch[which(AnalysisDF$nopartymismatch == "Own Party")] <- "no_mismatch"
                              AnalysisDF$nopartymismatch[which(AnalysisDF$nopartymismatch == "2nd Party")] <- "no_mismatch"
                              AnalysisDF$nopartymismatch[which(AnalysisDF$nopartymismatch == "No Party")] <- "no_mismatch"
                             
table(AnalysisDF$nopartymismatch)

####################################################################################

#Adding Dummy to indicate if a gender match or mismatch between participant and presented fictional politician occurs

#adding label

AnalysisDF$GenderMatch  <- ifelse( (AnalysisDF$Gender == "Weiblich" & AnalysisDF$Gender_politician == "Male") | 
                       (AnalysisDF$Gender == "Maennlich" & AnalysisDF$Gender_politician == "Female"), "mismatch", "match" ) 

####################################################################################


#Adding Dummy to indicate treatments with image or without image

#adding label
table(AnalysisDF$NameTreatment)
AnalysisDF$image_check <- as.character(AnalysisDF$NameTreatment)
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_233_left")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_333_left")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_322_left")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_311-left")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_233_left")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_333_left")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_322_left")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_133_left")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_123-left")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_311-left")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_133-left")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_333_right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_311_right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_123_right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_333_right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_133_right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_311-right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_233_right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_123_right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_322_right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_133_right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_322_right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_233_right")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_222")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_122")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_123")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_122")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_222")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_211")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_111")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_111")] <- "no_image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_211")] <- "no_image"

AnalysisDF$image_check[which(AnalysisDF$image_check == "female_211_image")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_111_image")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_311_image")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_111_image")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_311_image-l")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_311_image_r")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_211_image")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_311_right_image")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_311_image_right")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_311_image_right")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_311_image_left")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "male_311_image_right")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_311_image_right")] <- "image"
AnalysisDF$image_check[which(AnalysisDF$image_check == "female_311_image_left")] <- "image"


table(AnalysisDF$image_check)

####################################################################################

#adding Dummy to merge treatments into general treatments without gender indication

table(AnalysisDF$NameTreatment)
AnalysisDF$treatment_nogender <- as.character(AnalysisDF$NameTreatment)
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_233_left")] <- "233_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_333_left")] <- "333_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_322_left")] <- "322_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_311-left")] <- "311_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_233_left")] <- "233_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_333_left")] <- "333_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_322_left")] <- "322_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_133_left")] <- "133_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_123-left")] <- "123_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_311-left")] <- "311_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_133-left")] <- "133_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_333_right")] <- "333_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_311_right")] <- "311_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_123_right")] <- "123_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_333_right")] <- "333_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_133_right")] <- "133_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_311-right")] <- "311_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_233_right")] <- "233_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_123_right")] <- "123_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_322_right")] <- "322_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_133_right")] <- "133_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_322_right")] <- "322_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_233_right")] <- "233_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_222")] <- "222"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_122")] <- "122"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_123")] <- "123"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_122")] <- "122"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_222")] <- "222"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_211")] <- "211"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_111")] <- "111"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_111")] <- "111"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_211")] <- "211"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_211_image")] <- "211_image"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_111_image")] <- "111_image"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_311_image")] <- "311_image"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_111_image")] <- "111_image"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_311_image-l")] <- "311_image_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_311_image_r")] <- "311_image_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_211_image")] <- "211_image"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_311_right_image")] <- "311_image_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_311_image_right")] <- "311_image_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_123_left")] <- "123_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_311_image_left")] <- "311_image_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_311_left")] <- "311_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_123_left")] <- "123_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_133_left")] <- "133_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_311_image_left")] <- "311_image_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_311_image_right")] <- "311_image_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_311_right")] <- "311_right"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "male_311_left")] <- "311_left"
AnalysisDF$treatment_nogender[which(AnalysisDF$treatment_nogender == "female_311_image_left")] <- "311_image_left"

table(AnalysisDF$treatment_nogender)

################################################################################################################

#adding Dummy which merges treatments together without distinction if image or not

table(AnalysisDF$treatment_nogender)
AnalysisDF$treatment_merged <- as.character(AnalysisDF$treatment_nogender)
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "111")] <- "111"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "111_image")] <- "111"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "122")] <- "122"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "123")] <- "123"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "123_left")] <- "123"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "123_right")] <- "123"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "133_left")] <- "133"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "133_right")] <- "133"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "211")] <- "211"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "211_image")] <- "211"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "222")] <- "222"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "233_left")] <- "233"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "233_right")] <- "233"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "311_image")] <- "311"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "311_image_left")] <- "311"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "311_image_right")] <- "311"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "311_left")] <- "311"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "311_right")] <- "311"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "322_left")] <- "322"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "322_right")] <- "322"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "333_left")] <- "333"
AnalysisDF$treatment_merged[which(AnalysisDF$treatment_merged == "333_right")] <- "333"

table(AnalysisDF$treatment_merged)


####################################################################################

#adding Dummy to compare w/ image VS w/o image

table(AnalysisDF$treatment_nogender)
AnalysisDF$treatment_compare <- as.character(AnalysisDF$treatment_nogender)
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "111")] <- "111"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "111_image")] <- "111_image"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "122")] <- "122"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "123")] <- "123"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "123_left")] <- "123"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "123_right")] <- "123"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "133_left")] <- "133"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "133_right")] <- "133"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "211")] <- "211"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "211_image")] <- "211_image"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "222")] <- "222"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "233_left")] <- "233"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "233_right")] <- "233"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "311_image")] <- "311_image"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "311_image_left")] <- "311_image"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "311_image_right")] <- "311_image"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "311_left")] <- "311"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "311_right")] <- "311"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "322_left")] <- "322"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "322_right")] <- "322"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "333_left")] <- "333"
AnalysisDF$treatment_compare[which(AnalysisDF$treatment_compare == "333_right")] <- "333"

table(AnalysisDF$treatment_compare)

####################################################################################

#adding a Dummy for social media expertise

table(AnalysisDF$SocialMediaUse)
AnalysisDF$socialmedia_competence <- as.character(AnalysisDF$SocialMediaUse)
AnalysisDF$socialmedia_competence[which(AnalysisDF$socialmedia_competence == "mehrmals pro Tag")] <- "high"
AnalysisDF$socialmedia_competence[which(AnalysisDF$socialmedia_competence == "konstant")] <- "high"
AnalysisDF$socialmedia_competence[which(AnalysisDF$socialmedia_competence == "etwa einmal im Monat")] <- "intermediate"
AnalysisDF$socialmedia_competence[which(AnalysisDF$socialmedia_competence == "mehrmals pro Woche")] <- "intermediate"
AnalysisDF$socialmedia_competence[which(AnalysisDF$socialmedia_competence == "etwa jede Woche")] <- "intermediate"
AnalysisDF$socialmedia_competence[which(AnalysisDF$socialmedia_competence == "etwa jede Stunde")] <- "high"

table(AnalysisDF$socialmedia_competence)



####################################################################################

#adding Dummy if participants follow a Politician on Social Media

table(AnalysisDF$Follow_Politician)
AnalysisDF$contact_politician <- as.character(AnalysisDF$Follow_Politician)
AnalysisDF$contact_politician[which(AnalysisDF$contact_politician == "Ja")] <- "YES"
AnalysisDF$contact_politician[which(AnalysisDF$contact_politician == "Nein")] <- "NO"
AnalysisDF$contact_politician[which(AnalysisDF$contact_politician == "weiss nicht")] <- "NO"

table(AnalysisDF$contact_politician)

####################################################################################

#adding Dummy for Education Level of Participants

table(AnalysisDF$Education)
AnalysisDF$Educ_Level <- as.character(AnalysisDF$Education)
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Universitaet und ETH (Bachelor)")] <- "High"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Universitaet und ETH (Master)")] <- "High"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Berufslehre")] <- "Middle"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Hoehere Fachschule")] <- "Middle"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Paedagogische Hochschule")] <- "High"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Sekundarstufe I (Bezirksschule, Sekundarschule, Realschule)")] <- "Low"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Fachhochschule (Bachelor)")] <- "High"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Doktorat")] <- "High"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Sekundarstufe II (Mittelschule, Kantonsschule, Gymnasium)")] <- "Middle"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Berufslehre (mit Berufsmatura)")] <- "Middle"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Fachhochschule (Master)")] <- "High"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Eidgenoessisches Berufsattest EBA")] <- "Low"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Primarschule")] <- "Low"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "keine der genannten Optionen")] <- NA

#DE
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Gymnasium")] <- "Middle"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Berufs-/ Technische Oberschule")] <- "High"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Berufsakademie")] <- "Middle"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Realschule")] <- "Middle"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Universitaet")] <- "High"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Berufsschule (Duales System); Berufsfachschule")] <- "Middle"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Fachhochschule")] <- "High"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Doktorat / Promotion")] <- "High"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Fachoberschule")] <- "High"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Berufsaufbauschule und Berufsvorbereitungsjahr")] <- "Low"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Hauptschule")] <- "Low"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Abendgymnasium")] <- "Middle"
AnalysisDF$Educ_Level[which(AnalysisDF$Educ_Level == "Grundschule")] <- "Low"

educ_level <- table(AnalysisDF$Educ_Level)
educ_level


####################################################################################

# "Extremism" Dummy: measuring how far away a participant is from the middle category (50) on the left-right slider

hist(AnalysisDF$left_right_scale_1)
#dev.copy(png,'histogramm_left-right.png')
#dev.off()

AnalysisDF$Extremism <- ifelse(is.na(AnalysisDF$left_right_scale_1), NA ,abs(diff(AnalysisDF$left_right_scale_1, 50, na.rm= TRUE)))

hist(AnalysisDF$Extremism)
count(AnalysisDF$Extremism)
#dev.copy(png,'histogramm_extremism.png')
#dev.off()

############################################################################################################################################
############################################################################################################################################

noimagesDF <- filter(AnalysisDF,AnalysisDF$image_check=="no_image")
noimagesDF

#Plotting means of merged treatments w/o images

#ordering the treatments according the ranking
noimagesDF$treatment_merged <- factor(noimages_noideologyDF$treatment_merged,levels = c("111","211","311","122","123","133","222","322","233","333"))

measurements8 <- noimagesDF$polscale
treatments8 <- noimagesDF$treatment_merged

means8 <- aggregate(polscale~treatment_merged, mean,data=noimagesDF)
medians8 <- aggregate(polscale~treatment_merged, median,data=noimagesDF)

boxplot(measurements8~treatments8,
        data = noimagesDF,
        notch = FALSE,
        main = "Means of Treatments",
        xlab = "Treatments (w/o images)",
        ylab = "Evaluation Score (0-100)",
        col = "gray",
        border = "black"
)
points(1:10, means8$polscale, col = "red",lwd = 4)
text(1:10, medians8$polscale + 3, labels = round(means8$polscale,digits=1))
lines(1:10, means8$polscale, col = "red", lwd = 2.5)

#dev.copy(png,'merged_treatments_no_images.png')
#dev.off()



############################################################################################################################################
############################################################################################################################################

#SELECTING ONLY CASES WITH NO IDEOLOGICAL MISMATCH, NO PARTY-IDEOLOGY MISMTACH AND WITHOUT IMAGES

noimages_noideologyDF <- filter(AnalysisDF,noleftrightmismatch=="no_mismatch"& nopartymismatch=="no_mismatch" & image_check=="no_image")
noimages_noideologyDF


############################################################################################################################################
############################################################################################################################################


#making Dataframes for every country

AnalysisDF_DE <- AnalysisDF[AnalysisDF$country == "DE",]
AnalysisDF_CH <- AnalysisDF[AnalysisDF$country == "CH",]


############################################################################################################################################
############################################################################################################################################
                             
                                                        # !!!!!!!!
                                    #FROM HERE ALL GRAPHS WITH NO IDEOLOGICAL AND PARTY MISMATCH
                                                         # !!!!!!!!

#######################################################################################################################################
################################################### !! ANALYSIS!! ####################################################################
#######################################################################################################################################

#   COMMENT:    
#   this section contains the analysis of the data.

#######################################################################################################################################

#Distribution of Treatments with merged categories

library(ggplot2)

table(AnalysisDF$polscale)

counts <- table(AnalysisDF$polscale, AnalysisDF$treatment_merged)
barplot(counts, main = "Counts of Treatments merged into Overall categories", 
        xlab = "Treatments")

#dev.copy(png,'count treatments merged all.png')
#dev.off()


#######################################################################################################################################

#Plotting means of all merged treatments 
library(ggplot2)

#ordering the treatments according the ranking
AnalysisDF$treatment_merged <- factor(AnalysisDF$treatment_merged,levels = c("111","211","311","122","123","133","222","322","233","333"))

measurements <- AnalysisDF$polscale
treatments <- AnalysisDF$treatment_merged

means <- aggregate(polscale~treatment_merged, mean,data=AnalysisDF)
medians <- aggregate(polscale~treatment_merged, median,data=AnalysisDF)

boxplot(measurements~treatments,
        data = AnalysisDF,
        notch = FALSE,
        main = "Means of Treatments, w/ ideology mismatch",
        xlab = "Treatments (merged ALL, w/ images)",
        ylab = "Evaluation Score (0-100)",
        col = "gray",
        border = "black"
)
points(1:10, means$polscale, col = "red", lwd = 4)
text(1:10, medians$polscale + 3, labels = round(means$polscale,digits=1))
lines(1:10, means$polscale, col = "red", lwd = 2.5)

#dev.copy(png,'merged_treatments_ALL.png')
#dev.off()

#######################################################################################################################################
#######################################################################################################################################

#Plotting means of all merged treatments 
library(ggplot2)

#ordering the treatments according the ranking
AnalysisDF$treatment_merged <- factor(AnalysisDF$treatment_merged,levels = c("111","211","311","122","123","133","222","322","233","333"))

measurements <- AnalysisDF$polscale
treatments <- AnalysisDF$treatment_merged

means4 <- aggregate(polscale~treatment_merged, mean,data=AnalysisDF)
medians4 <- aggregate(polscale~treatment_merged, median,data=AnalysisDF)

boxplot(measurements~treatments,
        data = AnalysisDF,
        notch = FALSE,
        main = "Means of Treatments",
        xlab = "Treatments (merged ALL, w/ images)",
        ylab = "Evaluation Score (0-100)",
        col = "gray",
        border = "black"
)
points(1:10, means4$polscale, col = "red", lwd = 4)
text(1:10, medians4$polscale + 3, labels = round(means$polscale,digits=1))
lines(1:10, means4$polscale, col = "red", lwd = 2.5)

#dev.copy(png,'merged_treatments_ALL.png')
#dev.off()

#######################################################################################################################################
#######################################################################################################################################

#Plotting means of merged treatments w/o images

#ordering the treatments according the ranking
noimages_noideologyDF$treatment_merged <- factor(noimages_noideologyDF$treatment_merged,levels = c("111","211","311","122","123","133","222","322","233","333"))

measurements9 <- noimages_noideologyDF$polscale
treatments9 <- noimages_noideologyDF$treatment_merged

means9 <- aggregate(polscale~treatment_merged, mean,data=noimages_noideologyDF)
medians9 <- aggregate(polscale~treatment_merged, median,data=noimages_noideologyDF)

boxplot(measurements9~treatments9,
        data = noimages_noideologyDF,
        notch = FALSE,
        main = "Means of Treatments, no ideology mismatch",
        xlab = "Treatments (w/o images)",
        ylab = "Evaluation Score (0-100)",
        col = "gray",
        border = "black"
        )
points(1:10, means9$polscale, col = "red",lwd = 4)
text(1:10, medians9$polscale + 3, labels = round(means9$polscale,digits=1))
lines(1:10, means9$polscale, col = "red", lwd = 2.5)

#dev.copy(png,'merged_treatments_no_images.png')
#dev.off()


#######################################################################################################################################
################################################# !! LINEAR REGRESSION MODELS !! #####################################################
#######################################################################################################################################

#COMMENT:
# before running the regression models some preparation work for the different models

#######################################################################################################################################
#building a DF with only personal treatments
#this is used later in model 4 to control the influence of images in treatments

OnlyPersonalDF <- filter(AnalysisDF,AnalysisDF$treatment_merged=="111" | AnalysisDF$treatment_merged=="211" | AnalysisDF$treatment_merged=="311"
                         | AnalysisDF$treatment_merged=="122" | AnalysisDF$treatment_merged=="123"| AnalysisDF$treatment_merged=="133")
OnlyPersonalDF


#######################################################################################################################################
# adding a dummy which splits the sample into two groups: left - right. Everybody who has a value >= 50 is considered right


AnalysisDF$leftrightgroup <- ifelse(
                              (AnalysisDF$left_right_scale_1 >=50),
                               "Right",
                               "Left")


table(AnalysisDF$leftrightgroup)

####################################################################

# Here, the polscale variable can temporarily manually be set to its subdimensions (upon Natalie' request).

par(mfrow=c(2,3))
hist(AnalysisDF$polscale)
hist(AnalysisDF$suitability)
hist(AnalysisDF$L2V)
hist(AnalysisDF$Warmth)
hist(AnalysisDF$Credibility)
par(mfrow=c(1,1))

	#  AnalysisDF$polscale <- AnalysisDF$suitability
	  AnalysisDF$polscale <- AnalysisDF$L2V # be aware! we are doing likilhood to vote!
	#  AnalysisDF$polscale <- AnalysisDF$Warmth
	#  AnalysisDF$polscale <- AnalysisDF$Credibility




#######################################################################################################################################
#Building the models 

AnalysisDF$treatment_merged_num <- as.numeric(AnalysisDF$treatment_merged) - 5
AnalysisDF$treatment_merged_num_010 <- as.numeric(AnalysisDF$treatment_merged)
AnalysisDF$toomuch <- ifelse(AnalysisDF$treatment_merged == "111" | AnalysisDF$treatment_merged == "222" |AnalysisDF$treatment_merged == "333","all same","not all same")
AnalysisDF$toomuch <- factor(AnalysisDF$toomuch, levels = c("not all same", "all same"))

AnalysisDF$noleftrightmismatch <- factor(AnalysisDF$noleftrightmismatch, levels = c("no_mismatch", "mismatch"))
AnalysisDF$nopartymismatch <- factor(AnalysisDF$nopartymismatch, levels = c("no_mismatch", "mismatch"))
table(AnalysisDF$image_check)

model1 <- lmer(polscale~treatment_merged_num +
			  (1|country)
             ,data=AnalysisDF)

summary(model1)
library(stargazer)
stargazer(model1,type="text",covariate.labels = c("Treatments","All treatments same", "Image yes", "left-right mismatch"," Party mismatch"))
sd(noimages_noideologyDF$polscale)

AnalysisDF$Educ_Level <- factor(AnalysisDF$Educ_Level,levels = c("Middle", "Low", "High"))

table(country)
AnalysisDF$country <- factor(AnalysisDF$country,levels = c("DE", "CH"))



#demographics
model2 <- lmer(polscale~treatment_merged_num +
               Age +
               Gender +
               Educ_Level +
			   (1|country)
             ,data=AnalysisDF)

summary(model2)
library(stargazer)
stargazer(model1,model2,type="text")
#anova(model1,model2)

AnalysisDF$Follow_Politician <- factor(AnalysisDF$Follow_Politician,levels = c("Nein", "Ja", "weiss nicht"))
AnalysisDF$political_content <- factor(AnalysisDF$political_content,levels = c("Nein", "Ja", "weiss nicht"))

#demographics + other participant characteristics
model3 <- lmer(polscale~treatment_merged_num +
               Age +
               Gender +
               Educ_Level +
               I(left_right_scale_1-50) +
               Extremism +
               political_content +
               Follow_Politician +
               socialmedia_competence +
			   (1|country)
             ,data=AnalysisDF)

summary(model3)
library(stargazer)
stargazer(model1,model2,model3,type="text")
#anova(model1,model2)

   
#model with matches/mismatches

model4 <- lmer(polscale~treatment_merged_num +
               Age +
               Gender +
               Educ_Level +
               I(left_right_scale_1-50) +
               Extremism +
               political_content +
               Follow_Politician +
               socialmedia_competence +
               Gender_politician +
               GenderMatch +
               noleftrightmismatch +
               nopartymismatch +
			   (1|country)
             ,data=AnalysisDF)

summary(model4)
library(stargazer)
stargazer(model1,model2,model3,model4,type="text")


#model 4b

#model with matches/mismatches

model4b <- lmer(polscale~treatment_merged_num +
               Age +
               Gender +
               Educ_Level +
               I(left_right_scale_1-50) +
               Extremism +
               political_content +
               Follow_Politician +
               socialmedia_competence +
               Gender_politician +
               GenderMatch +
               noleftrightmismatch *nopartymismatch +
			   (1|country)
             ,data=AnalysisDF)

summary(model4b)
library(stargazer)
stargazer(model1,model2,model3,model4,model4b,type="text")
#anova(model1,model3)

## estimate the effect of there being an image

	table(AnalysisDF$treatment_merged)
	table(AnalysisDF$treatment_merged,AnalysisDF$image_check)

	AnalysisDFOnlyPersonal <- AnalysisDF[which(AnalysisDF$treatment_merged == "111"|AnalysisDF$treatment_merged == "211"|AnalysisDF$treatment_merged == "311"),]
	nrow(AnalysisDFOnlyPersonal)
	sum(table(AnalysisDF$treatment_merged,AnalysisDF$image_check)[1:3,]) # check!

	AnalysisDFOnlyPersonal$image_check <- factor(AnalysisDFOnlyPersonal$image_check ,levels=c("no_image","image"))

	modelimage <- lm(polscale~treatment_merged_num +
				   Age +
				   Gender +
				   Educ_Level +
				   I(left_right_scale_1-50) +
				   Extremism +
				   political_content +
				   Follow_Politician +
				   socialmedia_competence +
				   Gender_politician +
				   GenderMatch +
				   noleftrightmismatch +
				   nopartymismatch +
				   toomuch +
				   image_check,
				 ,data=AnalysisDFOnlyPersonal)
	summary(modelimage)

	coef(modelimage)
	betaimage <- coef(modelimage)[which(names(coef(modelimage)) == "image_checkimage")]

AnalysisDF$image_check_num <- ifelse(AnalysisDF$image_check == "image",1,0)
table(AnalysisDF$image_check_num,AnalysisDF$image_check)

# model with additional tweet characteristics

	AnalysisDF$obscurepartydummy <- factor(AnalysisDF$obscurepartydummy,levels=c("regular","obscure"))
	table(AnalysisDF$obscurepartydummy)
	table(AnalysisDF$nopartymismatch,AnalysisDF$obscurepartydummy)

	DRED <- AnalysisDF
	# DRED <- AnalysisDF[which(!is.na(AnalysisDF$abslmerdifformodel)),]
	

	model5 <- lmer(polscale~treatment_merged_num +
				   Age +
				   Gender +
				   Educ_Level +
				   I(left_right_scale_1-50) +
				   Extremism +
				   political_content +
				   Follow_Politician +
				   socialmedia_competence +
				   Gender_politician +
				   GenderMatch +
				   noleftrightmismatch +
			#	   nopartymismatch +
				   toomuch +
				   offset(I(betaimage*image_check_num)) +
				   prefincondition +
				   nopartytreatment +
			#	   obscurepartydummy +
			#	   (1|country) +
				   (nopartymismatch|country)
				   #(nopartymismatch|country)
				 ,data=DRED)

	summary(model5)
	coef(model5)
	anova(model5)
	ranef(model5)
	attr(ranef(model5, postVar = TRUE)[[1]], "postVar")
	attr(ranef(model5, condVar = TRUE)[[1]], "postVar")

	stargazer(model1,model2,model3,model5,type="text",intercept.bottom=FALSE)


# model with left-right distance on basis of positions
	
				# list of variables to take towards the model
				#	abslmerdifformodel
				#	table(AnalysisDF$obscurepartydummy) # ofcourse always has party left right position missing!
					table(AnalysisDF$obscurepartydummy,is.na(AnalysisDF$abslmerdifformodel))
					table(AnalysisDF$prefincondition)
					table(AnalysisDF$nopartytreatment)
	
	ADFCH <- AnalysisDF[which(AnalysisDF$country == "CH"),]
	ADFDE <- AnalysisDF[which(AnalysisDF$country == "DE"),]
	nrow(ADFCH)
	nrow(ADFDE)
	
	library(ggplot2)
	library(ggpubr)
	require(gridExtra)

		freq_median_ch  = median(ADFCH$abslmerdifformodel,na.rm=TRUE)
		plot1 <- ggplot(ADFCH, aes(abslmerdifformodel)) +
		geom_histogram() +
		labs(title="lmer distance resp. and pol. in Swisterland") +
		geom_vline(aes(xintercept = freq_median_ch), linetype = "dashed", size = 1) +
		geom_text(aes(x=freq_median_ch+0.2, label=round(freq_median_ch,2), y=200), colour="black", angle=0,size=5) 
		
		freq_median_de  = median(ADFDE$abslmerdifformodel,na.rm=TRUE)
		plot2 <- ggplot(ADFDE, aes(abslmerdifformodel)) +
		geom_histogram() +
		labs(title="lmer distance resp. and pol. in Germany") +
		geom_vline(aes(xintercept = freq_median_de), linetype = "dashed", size = 1) +
		geom_text(aes(x=freq_median_de+0.2, label=round(freq_median_de,2), y=200), colour="black", angle=0,size=5) 


		grid.arrange(plot1, plot2, ncol=2)
		
	model6 <- lmer(polscale~treatment_merged_num +
				   Age +
				   Gender +
				   Educ_Level +
				   I(left_right_scale_1-50) +
				   Extremism +
				   political_content +
				   Follow_Politician +
				   socialmedia_competence +
				   Gender_politician +
				   GenderMatch +
				   noleftrightmismatch +
				#   nopartymismatch +
				#   abslmerdifformodel +
				   toomuch +
				   offset(I(betaimage*image_check_num)) +
				   prefincondition +
				   nopartytreatment +
				#   (1|country) +
				#   (nopartymismatch|country) +
				   (abslmerdifformodel|country)
				 ,data=DRED)

	summary(model6)
	coef(model6)
#	confint(model6)
	ranef(model6)
	attr(ranef(model6, postVar = TRUE)[[1]], "postVar")


	stargazer(model1,model2,model3,model5,model6,type="text",intercept.bottom=FALSE)
	anova(model5,model6) # 
	anova(model6,model5) # model 5 is clearly the much better fit! (when the party mismatch dummy is dropped the model really is worse).

## getting a visualisation of the model predictions

PREDAT <- AnalysisDF[which(!is.na(AnalysisDF$polscale) & 
							!is.na(AnalysisDF$treatment_merged_num) & 
							!is.na(AnalysisDF$Age) & 
							!is.na(AnalysisDF$Gender) & 
							!is.na(AnalysisDF$Educ_Level) & 
							!is.na(AnalysisDF$left_right_scale_1) & 
							!is.na(AnalysisDF$Extremism) & 
							!is.na(AnalysisDF$political_content) & 
							!is.na(AnalysisDF$Follow_Politician) & 
							!is.na(AnalysisDF$socialmedia_competence) & 
							!is.na(AnalysisDF$Gender_politician) & 
							!is.na(AnalysisDF$GenderMatch) & 
							!is.na(AnalysisDF$noleftrightmismatch) & 
							!is.na(AnalysisDF$nopartymismatch) & 
							!is.na(AnalysisDF$toomuch) & 
							!is.na(AnalysisDF$image_check_num) & 
							!is.na(AnalysisDF$country) 
							),]
				nrow(PREDAT)

PREDAT$predictionsrandomslopemodel <- predict(model5)

PREDAT$countrytimespartymismatch <- paste(PREDAT$country,PREDAT$nopartymismatch,sep="-")
table(PREDAT$countrytimespartymismatch)

ggplot(PREDAT, aes(x=treatment_merged_num, y=polscale,color=countrytimespartymismatch)) +
					geom_jitter(size=0.75) + 
					geom_smooth(method = lm, se = TRUE) 

ggplot(PREDAT, aes(x=treatment_merged_num, y=predictionsrandomslopemodel,color=countrytimespartymismatch)) +
					geom_jitter(size=0.75) + 
					geom_smooth(method = lm, se = TRUE) 


# and when we manually calculcate the regression lines (lets use the observed values as the dots here)
	
		# delta method
						cbind(fixef(model5),seq(from=1,to=length(fixef(model5)),by=1))
						length(fixef(model5))
						
						CC <- as.matrix(coef(model5)$country)
						
						# non-quota factions - for Germany
							i = 1
							deltaMethod(model5,paste("x1+x2*",i,"+",CC[1,1],sep=""), parameterNames= paste("x", 1:length(fixef(model5)), sep=""))	
							
							resvecmatch <- vector()
							resvecmatchlb <- vector()
							resvecmatchub <- vector()
							for(i in 1:10)
							{
								resvecmatch[i] <- deltaMethod(model5,paste("x1+x2*",i,sep=""), parameterNames= paste("x", 1:length(fixef(model5)), sep=""))[[1]]
								resvecmatchlb
								resvecmatchub
							}
							
							DEresvecmismatch <- vector()
							DEresvecmismatchlb <- vector()
							DEresvecmismatchub <- vector()
							for(i in 1:10)
							{
								DEresvecmismatch[i] <- deltaMethod(model5,paste("x1+x2*",i,"+",CC[1,1],sep=""), parameterNames= paste("x", 1:length(fixef(model5)), sep=""))[[1]]	
							
							
							}
							

							CHresvecmismatch <- vector()
							CHresvecmismatchlb <- vector()
							CHresvecmismatchup <- vector()
							for(i in 1:10)
							{
								CHresvecmismatch[i] <- deltaMethod(model5,paste("x1+x2*",i,"+",CC[2,1],sep=""), parameterNames= paste("x", 1:length(fixef(model5)), sep=""))[[1]]	
								CHresvecmismatchlb[i]
								CHresvecmismatchup
							}
							
	# make dataframe with these results
	
		table(AnalysisDF$treatment_merged_num)
	
		est <- c(resvecmatch,resvecmatch,DEresvecmismatch,CHresvecmismatch)
		type <- c(rep("no_mismatch",10),rep("no_mismatch",10),rep("mismatch",10),rep("mismatch",10))
		treatment_merged_num <- c(seq(from=-4, to=5,by=1),seq(from=-4, to=5,by=1),seq(from=-4, to=5,by=1),seq(from=-4, to=5,by=1))
		country = c(rep("DE",10),rep("CH",10),rep("DE",10),rep("CH",10))
		B <- as.data.frame(cbind(est,type,treatment_merged_num,country))

		B$countrytimespartymismatch <- paste(B$country,B$type,sep="-")
		table(PREDAT$countrytimespartymismatch)
		B$countrytimespartymismatch <- ifelse(B$countrytimespartymismatch == "CH-no_mismatch" |
										      B$countrytimespartymismatch == "DE-no_mismatch",
											  "no_mismatch",
											  B$countrytimespartymismatch
											 )
		B
		
		# we also need to do this for the prediction data dataframe		
		PREDAT$countrytimespartymismatch <- ifelse(PREDAT$countrytimespartymismatch == "CH-no_mismatch" |
										      PREDAT$countrytimespartymismatch == "DE-no_mismatch",
											  "no_mismatch",
											  PREDAT$countrytimespartymismatch
											 )
		table(PREDAT$countrytimespartymismatch)
		
		B$est <- as.numeric(as.character(B$est))
		B$treatment_merged_num <- as.numeric(as.character(B$treatment_merged_num))
		B


		my_theme = theme(
									  axis.title.x = element_text(size = 16),
									  axis.text.x = element_text(size = 12),
									  axis.title.y = element_text(size = 16),
									  axis.text.y = element_text(size = 12),
									  legend.position="right",
									  legend.title = element_text(size = 14),
									  legend.text = element_text(size = 14)
						)
	
B$countrytimespartymismatch <- factor(B$countrytimespartymismatch,levels=c("no_mismatch","CH-mismatch","DE-mismatch")) 
PREDAT$countrytimespartymismatch <-	factor(PREDAT$countrytimespartymismatch,levels=c("no_mismatch","CH-mismatch","DE-mismatch"))
table(B$countrytimespartymismatch)

ggplot() +
	geom_jitter(data = PREDAT,aes(x=treatment_merged_num, y=polscale,shape=countrytimespartymismatch,color=countrytimespartymismatch),size=1.5,width = 0.15) +
	geom_line(data = B,aes(x=treatment_merged_num, y=est,color=countrytimespartymismatch),size=2.0) +
	scale_x_discrete(name ="Treatment: private to policy", 
                    limits=seq(from=-4,to=5,by=1),
					labels=c(111,211,311,122,123,133,222,322,233,333)
					) +
	scale_y_discrete(name="Self-reported likelihood to vote for candidate",
					limits=seq(from=0, to=100, by=10),
					labels=seq(from=0, to=100, by=10)
					) +
	scale_color_manual(name = "Estimated likelihood scores", 
						 labels = c("no party mismatch","CH party mismatch", "DE party mismatch"),
						 values=c("deeppink4", "grey7", "grey60")) +
	scale_shape_manual(name="Observed likelihood scores", 
					   labels = c("no party mismatch","CH party mismatch", "DE party mismatch"),
					   values = c(0, 1, 2)) +
	
	my_theme 
	

	
	
	
	table(PREDAT$treatment_merged_num)
	
	+ 
PREDAT, aes(x=treatment_merged_num, y=est,color=countrytimespartymismatch)) +
					geom_jitter(size=0.75) + 
					geom_line(aes())				
	

# getting the random part of the model setup and in
	
	m1 <- model1
	m2 <- model2
	m3 <- model3
	#m4 <- model4
	m5 <- model6
	m6 <- model6

# name replacements

	specificnamecleaning <- function(dirtynamesloc)	
			{
				cleanernames <- gsub("treatment_merged_num","Private (low end) or Policy (high end)",dirtynamesloc,fixed=TRUE)
				cleanernames <- gsub("Age","Age",cleanernames,fixed=TRUE)
				cleanernames <- gsub("GenderWeiblich","Gender (Female)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("Educ_LevelLow","Education Level (Low)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("Educ_LevelHigh","Education Level (High)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("I(left_right_scale_1 - 50)","Respondent left-right score",cleanernames,fixed=TRUE)
				cleanernames <- gsub("Extremism","Extremism",cleanernames,fixed=TRUE)
				cleanernames <- gsub("political_contentJa","Seen political content (Yes)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("political_contentweiss nicht","Seen political content (Don't know)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("Follow_PoliticianJa","Following a politician (Yes)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("Follow_Politicianweiss nicht","Following a politician (Don't know)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("socialmedia_competenceintermediate","Social Media Competence (interm.)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("Gender_politicianMale","Gender of politician (Male)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("GenderMatchmismatch","Gender mismatch",cleanernames,fixed=TRUE)
				cleanernames <- gsub("noleftrightmismatchmismatch","Content left-right mismatch",cleanernames,fixed=TRUE)
				cleanernames <- gsub("toomuchall same","All tweets of series same style",cleanernames,fixed=TRUE)
				cleanernames <- gsub("prefinconditionno pref in selected condition","No party preference",cleanernames,fixed=TRUE)
				cleanernames <- gsub("nopartytreatmentnone shown","Shown politician without party label",cleanernames,fixed=TRUE)

				return(cleanernames)
			}
			specificnamecleaning(dirtynames)


# use bootstrapping to get a standard error for the variance estimates.
		runconfints <- TRUE
		
				indivlvar <- format(round(c(
							as.data.frame(VarCorr(m1))$vcov[2],
							as.data.frame(VarCorr(m2))$vcov[2],
							as.data.frame(VarCorr(m3))$vcov[2],
							as.data.frame(VarCorr(m5))$vcov[4]
											),digits=3),nsmall=3)
		
		if (runconfints)
		{
				simulations <- 50
				am1 <- confint(m1,method="boot",nsim=simulations)
				
				am2 <- confint(m2,method="boot",nsim=simulations)
				
				am3 <- confint(m3,method="boot",nsim=simulations)
				am5 <- am3 # temp! take the confidence intervals from the previous run by default so that the script does not crash of no ci can be calculated
				am5 <- confint(m5,method="boot",nsim=simulations)

				indivlvarse <- format(round(c(
					((am1[2,2] - am1[2,1]) / 1.98),
					((am2[2,2] - am2[2,1]) / 1.98),
					((am3[2,2] - am3[2,1]) / 1.98),
					((am5[2,2] - am5[2,1]) / 1.98)
					),digits=3),nsmall=3)
		} else {
			indivlvarse <- rep("NE",4)
		}								
				countryvar <- format(round(c(
							as.data.frame(VarCorr(m1))$vcov[1],
							as.data.frame(VarCorr(m2))$vcov[1],
							as.data.frame(VarCorr(m3))$vcov[1],
							as.data.frame(VarCorr(m5))$vcov[1]
											),digits=3),nsmall=3)
		if (runconfints)
		{					
				countryvarse <- format(round(c(
					((am1[1,2] - am1[1,1]) / 1.98),
					((am2[1,2] - am2[1,1]) / 1.98),
					((am3[1,2] - am3[1,1]) / 1.98),
					((am5[1,2] - am5[1,1]) / 1.98)
					),digits=3),nsmall=3)
		} else {
			countryvarse <- rep("NE",4)
		}	

	nobsc <-  c(nobs(m1),
				nobs(m2),
				nobs(m3),
				nobs(m5),
				nobs(m6))
						
	nrofcountries <- c(sapply(ranef(m1),nrow)[1],
							sapply(ranef(m2),nrow)[1],
							sapply(ranef(m3),nrow)[1],
							sapply(ranef(m5),nrow)[1])

			GiveBrackets <- function(vector1)
				{
					resultvec <- vector()
					for(i in 1:length(vector1))
					{
						resultvec[i] <- paste("(",vector1[i],")",sep="")
					}
					return(resultvec)
				}


	varlabels <- specificnamecleaning(names(fixef(m5)))

	stargazer(
		m1,
		m2,
		m3,
		m5,
		type="latex",
		intercept.bottom=FALSE,
		no.space=TRUE,
		column.labels=(c("Treatment","Demographics etc","Oth. Respon.","Tweet.Char.")),
		star.char = c(".", "*", "**", "***"),
		star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
		keep.stat=c("ll"),
		omit.stat=c("aic","bic"),
		font.size = "small",
		label = "ObtPropRegTab",
		caption = "Logistic regression model predicting respondents' candidate evaluation on basis of tweet style and controls",
		dep.var.labels = c("Likelihood to vote"),
		covariate.labels = varlabels,
			add.lines = list(	
							c("Random effects"),
							c("--------------------------"),
							c("NR of respondents",nobsc),
							c("respondent-level var",indivlvar),
							c("",GiveBrackets(indivlvarse)),
							c("NR of countries",nrofcountries),
							c("country-level var",countryvar),
							c("",GiveBrackets(countryvarse))
							)
		  )


# and a standardised version 
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}
stdCoef.merMod(m5)


#######################################################################################################################################
#building a Table with descriptives of variables used in the regression models above

DF2 <- AnalysisDF
head(DF2)
summary(DF2)

varstotake2 <- c("Age","Gender","Gender_politician","Follow_Politician","political_content","Educ_Level","left_right_scale_1","Extremism",
                 "socialmedia_competence","GenderMatch","noleftrightmismatch","nopartymismatch","treatment_merged")
  

RegressionDF <- DF2[varstotake2]
head(RegressionDF)
summary(RegressionDF)

# set NA values
RegressionDF[RegressionDF=="NA"] <- NA
RegressionDF[RegressionDF=="#VALUE!"] <- NA


summary(head(RegressionDF, n=1322))
library(stargazer)
stargazer(as.data.frame(RegressionDF),type="text")

#######################################################################################################################################

#correlation of measurement items

library(corrplot)

varstotake2 <- c("suitability","L2V","Warmth","Credibility","Thermometer")

EvaluationDF <- DF[varstotake2]

 
M <- cor(EvaluationDF)
head(round(M,2))
test <- stargazer(M,type="text")

library("PerformanceAnalytics")

chart.Correlation(EvaluationDF, histogram=TRUE, pch=19)
#dev.copy(png,'Correlation_Evaluation_Items_PilotV2.png')
#dev.off()

#???corrplot(M, type = "upper")
#dev.copy(png,'Correlation_Evaluation_Items_PilotV2.png')
#dev.off()

library(psych)

bb <- alpha(EvaluationDF,keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
      check.keys=FALSE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,
      n.obs=NULL,impute=NULL)

summary(bb)



#######################################################################################################################################

#comparing treatments w/ image s w/o image (all treatment settings w/o ideology and party mismatch)


#making DF for comparison of w/ & w/o image treatments
prepDF1 <- AnalysisDF[AnalysisDF$treatment_compare %in% c("111","211","311","111_image","211_image","311_image"),]
prepDF1


comparingDF <- filter(prepDF1,noleftrightmismatch=="no_mismatch"& nopartymismatch=="no_mismatch")
comparingDF

#ordering the treatments according the ranking

comparingDF$treatment_compare <- factor(comparingDF$treatment_compare,levels = c("111","111_image","211","211_image","311","311_image"))


#plotting the treatments

measurements1 <- comparingDF$polscale
treatments1 <- comparingDF$treatment_compare


means1 <- aggregate(polscale~treatment_compare, mean,data=comparingDF)
means1
medians1 <- aggregate(polscale~treatment_compare, median,data=comparingDF)
medians1

boxplot(measurements1~treatments1,
        data = comparingDF,
        notch = FALSE,
        main = "Treatment w/ image VS w/o image (N=321)",
        xlab = "Treatments (no ideological or party mismatch)",
        ylab = "Evaluation Score (0-100)",
        col = "gray",
        border = "black"
)
points(1:6, means1$polscale, col = "red")
text(1:6, medians1$polscale + 4, labels = round(means1$polscale,digits=1))
lines(1:6, means1$polscale, col = "red", lwd = 2.5)

#dev.copy(png,'Image Vs No Image Treatments.png')
#dev.off()

###############################################################################################################

#adding the level of social media expertise to the comparison of images vs no images

#with high level of social media competence

comparingDF2 <- filter(prepDF1,noleftrightmismatch=="no_mismatch"& nopartymismatch=="no_mismatch" & socialmedia_competence == "high")
comparingDF2

#ordering the treatments according the ranking

comparingDF2$treatment_compare <- factor(comparingDF2$treatment_compare,levels = c("111","111_image","211","211_image","311","311_image"))
count(comparingDF2)

#plotting the treatments

measurements2 <- comparingDF2$polscale
treatments2 <- comparingDF2$treatment_compare

means2 <- aggregate(polscale~treatment_compare, mean,data=comparingDF2)
means2
medians2 <- aggregate(polscale~treatment_compare, median,data=comparingDF2)

boxplot(measurements2~treatments2,
        data = comparingDF2,
        notch = FALSE,
        main = "Treatment w/ image VS w/o image high competence (N=181)",
        xlab = "Treatments (no ideological or party mismatch)",
        ylab = "Evaluation Score (0-100)",
        col = "gray",
        border = "black"
)
points(1:6, means2$polscale, col = "red")
text(1:6, medians2$polscale + 4, labels = round(means2$polscale,digits=1))
lines(1:6, means2$polscale, col = "red", lwd = 2.5)

#dev.copy(png,'image-no_image high competence.png')
#dev.off()

#######################################################
#with intermediate level of social media competence

comparingDF3 <- filter(prepDF1,noleftrightmismatch=="no_mismatch"& nopartymismatch=="no_mismatch" & socialmedia_competence == "intermediate")
comparingDF3


#ordering the treatments according the ranking

comparingDF3$treatment_compare <- factor(comparingDF3$treatment_compare,levels = c("111","111_image","211","211_image","311","311_image"))
count(comparingDF3)

#plotting the treatments

measurements3 <- comparingDF3$polscale
treatments3 <- comparingDF3$treatment_compare

means3 <- aggregate(polscale~treatment_compare, mean,data=comparingDF3)
means3
medians3 <- aggregate(polscale~treatment_compare, median,data=comparingDF3)

boxplot(measurements3~treatments3,
        data = comparingDF3,
        notch = FALSE,
        main = "Treatments w/ image VS w/o image intermed. competence (N=114)",
        xlab = "Treatments (no ideological or party mismatch)",
        ylab = "Evaluation Score (0-100)",
        col = "gray",
        border = "black"
)
points(1:6, means3$polscale, col = "red")
text(1:6, medians3$polscale + 4, labels = round(means3$polscale,digits=1))
lines(1:6, means3$polscale, col = "red", lwd = 2.5)

#dev.copy(png,'image-no_image intermediate competence.png')
#dev.off()

##############################################################################################################
##############################################################################################################

#treatment merged ALL split by social media experience

#Ploting all merged Treatments with Participants with intermediate social media competence

comparingDF6 <- filter(AnalysisDF,noleftrightmismatch=="no_mismatch"& nopartymismatch=="no_mismatch" & socialmedia_competence == "intermediate")
comparingDF6

#ordering the treatments according the ranking
comparingDF6$treatment_merged <- factor(comparingDF6$treatment_merged,levels = c("111","211","311","122","123","133","222","322","233","333"))

measurements6 <- comparingDF6$polscale
treatments6 <- comparingDF6$treatment_merged

means6 <- aggregate(polscale~treatment_merged, mean,data=comparingDF6)
medians6 <- aggregate(polscale~treatment_merged, median,data=comparingDF6)

boxplot(measurements6~treatments6,
        data = comparingDF6,
        notch = FALSE,
        main = "Means of All Treatments intermediate competence",
        xlab = "Treatments (merged)",
        ylab = "Evaluation Score (0-100)",
        col = "gray",
        border = "black"
)
points(1:10, means6$polscale, col = "red")
text(1:10, medians6$polscale + 3, labels = round(means6$polscale,digits=1))
lines(1:10, means6$polscale, col = "red", lwd = 2.5)

#dev.copy(png,'merged_treatments_All_intermediate_experience.png')
#dev.off()

#####################################################################################################
#Ploting all merged Treatments with Participants with high social media competence

comparingDF5 <- filter(AnalysisDF,noleftrightmismatch=="no_mismatch"& nopartymismatch=="no_mismatch" & socialmedia_competence == "high")
comparingDF5

#ordering the treatments according the ranking
comparingDF5$treatment_merged <- factor(comparingDF5$treatment_merged,levels = c("111","211","311","122","123","133","222","322","233","333"))

measurements5 <- comparingDF5$polscale
treatments5 <- comparingDF5$treatment_merged

means5 <- aggregate(polscale~treatment_merged, mean,data=comparingDF5)
medians5 <- aggregate(polscale~treatment_merged, median,data=comparingDF5)

boxplot(measurements5~treatments5,
        data = comparingDF5,
        notch = FALSE,
        main = "Means of All Treatments high competence",
        xlab = "Treatments (merged)",
        ylab = "Evaluation Score (0-100)",
        col = "gray",
        border = "black"
)
points(1:10, means5$polscale, col = "red")
text(1:10, medians5$polscale + 3, labels = round(means5$polscale,digits=1))
lines(1:10, means5$polscale, col = "red", lwd = 2.5)

#dev.copy(png,'merged_treatments_All_high_experience.png')
#dev.off()



varstotakeA <- c("Age","Gender","Gender_politician","voting_kanton","Follow_Politician", 
                "SocialMediaPosting","left_right_scale_1","polscale","noleftrightmismatch","nopartymismatch","gendermatch","image_check",
                "socialmedia_competence","contact_politician","Educ_Level","Extremism","leftrightgroup")

VariableDF <- DF[varstotake]
head(VariableDF)
summary(VariableDF)
stargazer(VariableDF, type = "text")


count(AnalysisDF$party_treatment)
mean(AnalysisDF$Age)
