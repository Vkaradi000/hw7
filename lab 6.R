Victoria Karadimas
Lab 6
Amira, Zack, Liam, Muhibul

summary(Household_Pulse_data)

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA")

model_logit1 <- glm(vaxx ~ EEDUC, family = binomial, data = Household_Pulse_data)
summary(model_logit1)
table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)
pick_use1 <- (Household_Pulse_data$TBIRTH_YEAR < 2000) 
dat_use1 <- subset(Household_Pulse_data, pick_use1)


dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 
model_logit1 <- glm(vaxx ~ TBIRTH_YEAR + EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
summary(model_logit1)
new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1990,
                                       EEDUC = factor("bach deg", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("never",levels = levels(dat_use1$MS)),
                                       RRACE = factor("Black",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("male", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit1,new_data_to_be_predicted)

#My group and i wanted to to test out what gender is most likely to get vaxx
table(Household_Pulse_data$vaxx,Household_Pulse_data$GENID_DESCRIBE)
pick_use1 <- (Household_Pulse_data$GENID_DESCRIBE == "male") 
dat_use1 <- subset(Household_Pulse_data, pick_use1)
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 


pick_use1 <- (Household_Pulse_data$GENID_DESCRIBE == "female") 
dat_use1 <- subset(Household_Pulse_data, pick_use1)
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 


model_logit2 <- glm(vaxx ~ GENID_DESCRIBE,
                    family = binomial, data = Household_Pulse_data)
summary (model_logit2)

new_data_to_be_predicted2 <- data.frame(GENID_DESCRIBE = "male", 
                                        GENID_DESCRIBE = "female", data = dat_use1)
predict(model_logit2,new_data_to_be_predicted)

#after comparing genders we wanted to test out other factors
new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1990,
                                       EEDUC = factor("bach deg", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("never",levels = levels(dat_use1$MS)),
                                       RRACE = factor("Black",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("male", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit1,new_data_to_be_predicted)


new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1996,
                                       EEDUC = factor("some coll", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("married",levels = levels(dat_use1$MS)),
                                       RRACE = factor("white",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Not Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("Female", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit1,new_data_to_be_predicted)


#The Virus That Devastated Tourism: The Impact of Covid-19 on the Housing Market 
#BY Batalha, Mafalda; Goncalves, Duarte; Peralta, Susana; Pereira dos Santos, Joao
#this article describes the negative impacts covid caused on rentals. The authours compared pre-pandemic VS. 2018 rentals.
#the results came back sating prices decreased by 4.1%


#  https://www.census.gov/data/tables/2022/demo/hhp/hhp50.html
#data set for housing tables, they are comparing different date ranges and the amount people were able to afford on monthly payments. This is mostly based on mortagges instead of rentals. But we are getting closer :) 
