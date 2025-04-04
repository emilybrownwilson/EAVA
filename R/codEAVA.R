#' @title codEAVA
#' @description Assigns cause of death by Expert Algorithm Verbal Autopsy
#' @param df A data frame frame with 2016 WHO VA responses in openVA input format
#' @param age_group Age group input, either "neonate" or "child"
#' @returns A two-column data frame with unique identifier and cause of death
#' @export
codEAVA <- function(df,age_group)
{

  if(age_group=="neonate"){
    data <- df
    data$Stillbirth <- ifelse(data$i104o %in% c("n",".") & data$i109o %in% c("n",".") & data$i110o %in% c("n","."),1,0)
    data <- data[data$Stillbirth!=1,]

    data <- subset(data, age <28)
    dim(data)

    ## |++| neonatal tetanus1 |++|
    data$nnt1 <- ifelse((data$age > 2 & data$age < 27) & data$i219o == "y" & ((data$i271o == "y" & data$i273o == "y") | (data$i106a == "y" & data$i107o == "y")),1,2)
    table(data$nnt1, exclude = NULL)

    ## | + congenital malformation2 + |
    data$congmalf2 <- ifelse(data$i373o == "y" | data$i372o == "y" | data$i371o == "y" | data$i370o == "y", 1,2)
    table(data$congmalf2, exclude = NULL)

    ## |++| birth injury |++| (neonates alone - i.e., not with stillbirths)
    data$bi5 <- ifelse(data$i115o %in% "y", 1,2)
    table(data$bi5, exclude = NULL)

    ## |++| birth asphyxia5 |++|;
    data$ba5 <- ifelse((data$i106a == "y" | data$i111o == "n") & ((data$i271o == "n" | data$i219o == "y" | data$i275o == "y" | data$i276o == "y" | data$i286o == "y") | data$age == 0) ,1,2)
    table(data$ba5, exclude = NULL)

    # * |++| preterm with rds (in months) |++|;
    data$preterm_rds_mo <- ifelse((data$i367b == "y" | data$i367c == "y" ) & (data$i166o == "y" & data$fb_day0 == "y" & data$i147o == "n" & data$i284o == "n"),1,2)
    table(data$preterm_rds_mo, exclude = NULL)

    # * |++| preterm_all (in months) |++|;
    data$preterm_all_mo <- ifelse((data$i367b %in% "y" & data$i166o %in% "y" & data$fb_day0 == "y" & data$i147o %in% "n" & data$i284o %in% "n") | data$i367c %in% "y", 1, 2)
    table(data$preterm_all_mo, exclude = NULL)

    # * |++| meningitis451 |++|	;
    data$meningitis451 <- ifelse(data$i147o %in% "y" & (data$i278o %in% "y" | data$i219o %in% "y" | data$i275o %in% "y" | data$i276o %in% "y") & (data$i286o %in% "y" | data$i281o %in% "y"), 1,2)
    table(data$meningitis451, exclude = NULL)

    # * |++| meniningitis451 without nnt1 |++| (used to avoid unrealistic comorbidity with neonatal tetanus);
    data$meningitis451_nonnt1 <- ifelse(data$meningitis451 %in% 1 & data$nnt1 %in% 2, 1, 2)
    table(data$meningitis451_nonnt1, exclude = NULL)

    # * |++| diarrhea8 |++| (for NNs: acute diarrhea with >4 stools on worst day);
    data$diarrhea8 <- ifelse(data$i181o == "y" & data$i183b=="y", 1,2)
    table(data$diarrhea8, exclude = NULL)

    # * |++| pneumonia157 |++|;
    data$pneumo157sign1 <- ifelse(data$i166o == "y", 1,0)                        # fast breathing;
    data$pneumo157sign2 <- ifelse(data$i172o == "y", 1,0)                        # CI;
    data$pneumo157sign3 <- ifelse(data$i173b == "y", 1,0)  # grunting;
    data$pneumo157sign4 <- ifelse(data$i271o == "n" | data$i273o == "y", 1,0)    # not able to suckle on day 1 or stopped suckling;
    data$pneumo157sign5 <- ifelse(data$i104o == "n" | data$i107o == "y", 1,0)    # never cried or stopped crying;

    data$pneumo157signs <- data$pneumo157sign2 + data$pneumo157sign3 + data$pneumo157sign5

    data$pneumonia157 <- ifelse((data$i166o == "y" & data$i167c == "y") | data$i159o %in% "y" & data$i161b == "y" & data$pneumo157signs>1, 1, 2)
    table(data$pneumonia157, exclude = NULL)

    # * |++| sepsisfvr |++|;
    data$sepsisfvr <- ifelse(data$i147o %in% "y" | data$i284o %in% "y", 1,2)
    table(data$sepsisfvr, exclude = NULL)

    # * |++| sepsisfvr2_2 |++| ;
    data$sepsisfvr2sign1 <- ifelse(data$i147o == "y" | data$i284o == "y", 1,0)   # (fever/cold-to-touch);
    data$sepsisfvr2sign3 <- ifelse(data$i271o == "n" | data$i273o == "y", 1,0)   # (no normal day1 suckle/stopped suckle);
    data$sepsisfvr2sign5 <- ifelse(data$i219o == "y", 1,0)                       # (convulsion);
    # data$sepsisfvr2sign6 <- ifelse(data$id10188=="yes", 1,0)                   # (vomited everything);
    data$sepsisfvr2sign7 <- ifelse(data$i289o == "y" | data$i265o == "y", 1, 0)  # (jaundice);
    data$sepsisfvr2sign9 <- ifelse(data$i107o == "y", 1,0)                       # (stopped cry);
    data$sepsisfvr2sign10 <- ifelse(data$i286o == "y" | data$i281o == "y", 1,0)  # (lethargic/unconscious);
    data$sepsisfvr2sign12 <- ifelse(data$i172o == "y" | data$i173b == "y", 1,0)# (CI/grunt);
    # data$LBI <- ifelse(data$q5030==1 | data$id10287=="yes" | data$q5033==1 | data$id10227=="yes" | data$id10240=="yes", 1,2) # with jaundice;
    # data$sepsisfvr2sign14 <- ifelse(data$LBI==1,1,0)

    data$sepsisfvr2signs = data$sepsisfvr2sign1 + data$sepsisfvr2sign3 + data$sepsisfvr2sign5 + data$sepsisfvr2sign9 + data$sepsisfvr2sign10 + data$sepsisfvr2sign12
    data$sepsisfvr2_2 <- ifelse(data$sepsisfvr == 1 | data$sepsisfvr2signs>1, 1,2)
    table(data$sepsisfvr2_2, exclude = NULL)

    # * |++| sepsisfvr2_2 without neonatal tetanus |++|;
    data$sepsisfvr2_2_nonnt1 <- ifelse(data$sepsisfvr2_2 == 1 & data$nnt1 == 2, 1, 2)
    table(data$sepsisfvr2_2_nonnt1, exclude = NULL)

    # * |++| possible pneumonia |++|;
    data$possiblepneumonia9 <- ifelse(data$i159o == "y" & data$sepsisfvr2_2 == 1 & data$pneumonia157 == 2, 1, 2)
    table(data$possiblepneumonia9, exclude = NULL)

    # * |++| jaundice |++|;
    data$jaundice2 <- ifelse((data$i289o == "y" | data$i265o == "y") & (data$i273o == "y" | data$i286o == "y" | data$i215o == "y" | data$i282o == "y" | data$i283o == "y") & (data$i147o == "n" & data$i284o == "n"), 1, 2)
    table(data$jaundice2, exclude = NULL)

    # * |++| hemorrhagic disease of the newborn |++|;
    data$hemorrhageNN <- ifelse(data$i241o == "y" & data$i147o == "n" & data$i284o == "n", 1, 2)
    table(data$hemorrhageNN, exclude = NULL)

    # * |++| SUID (sudden unexplained infant death) |++|;
    data$suid <- ifelse(data$i290o == "y" & data$i115o != "y" & data$i370o != "y" & data$i111o != "n" & data$i112o != "y" & data$i113o != "y" &
                          (data$i105o != "n" | data$i106a == "n") & data$i107o != "y" & data$i271o != "n" & data$i273o != "y" & data$i159o != "y" &
                          data$i166o != "y" & data$i172o != "y" & data$i173b != "y" &
                          data$i219o != "y" & data$i147o != "y" & data$i284o != "y" & data$i286o != "y" & data$i281o != "y" &
                          data$i278o != "y" & data$i287o != "y" & data$i288o != "y" & data$i240o != "y" &
                          data$i239o != "y" & data$i241o != "y" & data$i181o != "y" & data$i188o != "y" &
                          data$i289o != "y" & data$i265o != "y", 1, 2)
    table(data$suid, exclude = NULL)

    # Neonatal Hieararcy (Compromise)
    data$allexpertdxs <- NA
    data$allexpertdxs[is.na(data$allexpertdxs) & data$nnt1==1] <- "NNT"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$congmalf2==1] <- "Malformation"
    data$allexpertdxs[is.na(data$allexpertdxs) & (data$ba5==1 | data$bi5==1)] <- "Intrapartum"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$preterm_all_mo==1] <- "Preterm"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$meningitis451_nonnt1==1] <- "Meningitis"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$diarrhea8==1] <- "Diarrhea"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$pneumonia157==1] <- "Pneumonia"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$possiblediar8_8==1] <- "Diarrhea"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$possiblepneumonia9==1] <- "Pneumonia"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$sepsisfvr2_2_nonnt1==1] <- "Sepsis"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$jaundice2==1] <- "Other"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$hemorrhageNN==1] <- "Other"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$suid==1] <- "Other"
    data$allexpertdxs[is.na(data$allexpertdxs)] <- "Unspecified"
  }

  if(age_group=="child"){
    data <- df
    data <- subset(data, age >= 28 & age < 60*30.4)

    # * Malnutrition1 (residual malnutrition – placed at the bottom of the hierarchy);
    data$malnutrition1 <- ifelse(data$i244o %in% "y" | data$i249o %in% "y",1,2)
    table(data$malnutrition1, exclude = NULL)

    # * |++| Malnutrition2 |++|; (modified algorithm: when SA data on the order of symptoms appearance are not available);
    data$malnutrition2 <- ifelse(data$i249o == "y" & data$i250b >= data$i120c,1,2)
    table(data$malnutrition2, exclude = NULL)

    # * |++| AIDS5 |++|
    data$a <- ifelse(data$i244o %in% "y",1,0) # thin limbs
    data$b <- ifelse(data$i200o %in% "y",1,0) # protruding belly
    data$c <- ifelse(data$i256o %in% "y",1,0) # swelling in armpits
    data$d <- ifelse(data$i245o %in% "y",1,0) # whitish rash in mouth
    data$e1 <- ifelse(data$i181o %in% "y" & data$i182d == "y",1,0) # liquid stools >30 days
    # data$e2 <- ifelse(data$id10181 %in% "yes" & data$id10182>14 & data$id10182!=99 & !is.na(data$id10182),1,0) # liquid stools 15-30 days
    data$f1 <- ifelse(data$i147o %in% "y" & data$i148d == "y",1,0) # fever >30 days
    # data$f2 <- ifelse(data$id10147 %in% "yes" & data$id10148>14 & data$id10148!=99 & !is.na(data$id10148),1,0) # fever >14 days
    data$g1 <- ifelse(data$i233o %in% "y" & data$i234c == "y",1,0) # skin rash >30 days
    # data$g2 <- ifelse(data$id10233 %in% "yes" & data$id10234>14 & data$id10234!=99 & !is.na(data$id10234),1,0) # skin rash >14 days
    ##
    data$h <- ifelse(data$i166o %in% "y",1,0) # fast breathing
    data$i <- ifelse(data$i172o %in% "y",1,0) # chest indrawing

    # data$J=data$a+data$b+data$c+data$d+data$e1+data$f1+data$g1+data$h+data$i
    data$K=data$a+data$b+data$e1+data$f1+data$g1+data$h+data$i
    # data$L=data$a+data$b+data$e2+data$f2+data$g2+data$h+data$i


    data$AIDS5 <- ifelse(data$i127o == "y",1,
                         ifelse((data$i126o == "yes" | data$i446o == "yes") & data$K>2,1,
                                ifelse(((data$i126 %in% c("dk","ref") & data$i446o %in% c("dk","ref")) |
                                          (data$i126o %in% c("dk","ref") & data$i446o == c("no")) |
                                          (data$i126o == c("no") & data$i446o %in% c("dk","ref"))) &
                                         (data$i256o == c("y") | data$i245o == c("y")) & data$K>2,1,2)))
    table(data$AIDS5, exclude = NULL)

    # * |++| Diarrhea8 |++| (acute diarrhea with >4 stools on worst day, or diarrhea >14 days [both without blood]);
    data$diarrhea8 <- ifelse((data$i181o == "y" & data$i183c == "y" & data$i186o == "n") |
                               (data$i181o == "y" & data$i182e == "y" & data$i186o == "n"),1,2)
    table(data$diarrhea8, exclude=NULL)

    # * |++| Dysentery8 |++| (bloody diarrhea with >4 stools on worst day, or bloody diarrhea >14 days);
    data$dysentery8 <- ifelse((data$i181o == "y" & data$i183c == "y" & data$i186o == "y") |
                                (data$i181o == "y" & data$i182e == "y" & data$i186o == "y"), 1,2)
    table(data$dysentery8, exclude=NULL)

    # * |++| Diarrhea8 OR Dysentery8 |++|
    data$diardysn8 <- ifelse(data$diarrhea8 == 1 | data$dysentery8 == 1,1,2)
    table(data$diardysn8, exclude=NULL)

    # * |++| possible diarrhea |++|;
    data$possiblediar8_4 <- ifelse(data$i181o %in% "y" & (data$i147o %in% "y" | data$i219o %in% "y" | data$i218 %in% "y") & data$i186o %in% "n" & data$diarrhea8 %in% 2, 1,2)
    table(data$possiblediar8_4, exclude=NULL)

    # * |++| possible dysentery |++|;
    data$possibledysn8_4 <- ifelse(data$i181o %in% "y" & (data$i147o %in% "y" | data$i219o %in% "y" | data$i218o %in% "y") & data$i186o %in% "y" & data$dysentery8 %in% 2, 1,2)
    table(data$possibledysn8_4, exclude=NULL)

    # * |++| possible diarrhea/dysentery |++|;
    data$possdiardysn8_4 <- ifelse(data$possiblediar8_4 == 1 | data$possibledysn8_4 == 1, 1,2)
    table(data$possdiardysn8_4, exclude=NULL)

    # * |++| Hemorrhagic fever |++|;
    data$hemfever <- ifelse(data$i147o %in% "y" & (data$i241o %in% "y" | data$i239o %in% "y"), 1,2)
    table(data$hemfever, exclude=NULL)

    # * |++| Malaria251 |++|;
    data$malaria251 <- ifelse((data$i147o == "y" & data$i149o %in% "yes" & data$i151b == "y" & data$i208o == "n" & ((data$age<365.25 & data$i278o %in% "n") | data$age>=365.25) & (data$i268o %in% "y" | data$i159o %in% "y" | data$i219o %in% "y" | data$i218o %in% "y")) |
                                (data$i147o %in% c("y") & data$i149o %in% c("y") & data$i150a %in% "y" & data$i208o %in% "n" & ((data$age<365.25 & data$i278o %in% "n") | data$age>=365.25) & (data$i268o %in% "y" | data$i219o %in% "y" | data$i218o %in% "y")), 1,2)
    table(data$malaria251, exclude = NULL)

    # * Measles4;
    data$measles4 <- ifelse(data$age>=120 & (data$i147o == "y" & data$i148e == "y") &
                              (data$i233o == "y" & data$i234d>2) & (data$i235a == "y" | data$i235d == "y"), 1, 2)
    table(data$measles4, exclude=NULL)

    # * Meningitis/encephalitis;
    data$meningitis <- ifelse(data$i147o == "y" & (data$i208o == "y" | data$i278o == "y"), 1,2)
    table(data$meningitis, exclude=NULL)

    # * |++| Pertussis |++|;
    data$pertussis <- ifelse((data$i153o == "y" & data$i154c == "y") & (data$i156o == "y" | data$i158o == "y" | data$i173b == "y"), 1,2)
    table(data$pertussis, exclude=NULL)

    # * |++| VA_pneumoniafb2daysgr |++|;
    data$pneumoniafb2daysgr <- ifelse(((data$i153o %in% "y" & data$i154d == "y") | (data$i159o %in% "y" & data$i161a == "y")) & ((data$i166o %in% "y" & data$i167d == "y") | data$i172o == "y" | data$i173c == "y"), 1,2)
    table(data$pneumoniafb2daysgr, exclude = NULL)

    # * |++| possibleari3 |++|;
    data$possibleari3 <- ifelse((data$i153o %in% "y" | data$i159o %in% "y" | (data$i166o %in% "y" & (data$i172o %in% "y" | data$i173c %in% "y"))) &
                                  (data$i156o %in% "y" | data$i166o %in% "y" | data$i172o %in% "y" | data$i173c %in% "y" |
                                     data$i147o %in% "y" | data$i219o %in% "y" | data$i218o %in% "yes") & data$pertussis %in% 2 & data$pneumoniafb2daysgr %in% 2, 1,2)
    table(data$possibleari3, exclude=NULL)

    # * Sepsis wo/malaria251;
    data$sepsis_nomal251 <- ifelse(data$i147o %in% "y" & (data$i235b == "y" | data$i235d == "y" | data$i219o %in% "y" | data$i218o %in% "y") & data$malaria251 %in% 2, 1,2)
    table(data$sepsis_nomal251, exclude=NULL)


    # * Residual infection;
    data$residual_infect_slide15_4 <- ifelse(data$i147o %in% "y" & (data$AIDS5 %in% 2 & data$measles4 %in% 2 & data$meningitis %in% 2 & data$malaria251 %in% 2 & data$dysentery8 %in% 2 & data$diarrhea8 %in% 2 &
                                                                      data$pertussis %in% 2 & data$pneumoniafb2daysgr %in% 2 & data$sepsis_nomal251 %in% 2 & data$possibledysn8_4 %in% 2 & data$possiblediar8_4 %in% 2 &
                                                                      data$possibleari3 %in% 2 & data$hemfever %in% 2), 1,2)
    table(data$residual_infect_slide15_4, exclude=NULL)


    # * Injury;
    data$injury <- ifelse(data$i077o %in% "y",1,2)
    table(data$injury, exclude=NULL)



    # * |++| Injury3_slide15 |++| (modified algorithm: when SA data on the order of symptoms appearance are not available);
    '%!in%' <- function(x,y)!('%in%'(x,y))
    data$injury3_slide15_4 <- ifelse((data$injury == 1 & data$i120d == "y") |
                                       (data$injury == 1 & data$AIDS5 != 1 & data$measles4 != 1 & data$meningitis != 1 & data$dysentery8 != 1 & data$diarrhea8 != 1 & data$pneumoniafb2daysgr != 1 & data$malaria251 != 1 &
                                          data$possibledysn8_4 != 1 & data$possiblediar8_4 != 1 & data$possibleari3 != 1 & data$hemfever != 1 & data$sepsis_nomal251 != 1 & data$residual_infect_slide15_4 != 1) |
                                       (data$injury == 1 & data$i147o == "y"), 1,2)

    table(data$injury3_slide15_4, exclude=NULL)

    #################################################### neonatal causes in 1to59m
    ## | + congenital malformation2 + | ;
    data$congmalf2 <- ifelse(data$i373o == "y" | data$i372o == "y" | data$i371o == "y" | data$i370o == "y", 1,2)
    table(data$congmalf2, exclude = NULL)

    ## |++| birth injury |++| (neonates alone - i.e., not with stillbirths) ;
    data$bi5 <- ifelse(data$age < 4*30.4 & data$i115o %in% "y", 1,2)
    table(data$bi5, exclude = NULL)

    ## |++| birth asphyxia5 |++|;
    data$ba5 <- ifelse(data$age < 4*30.4 & ((data$i106a == "y" | data$i111o == "n") & ((data$i271o == "n" | data$i219o == "y" | data$i275o == "y" | data$i276o == "y" | data$i286o == "y") | data$age == 0)) ,1,2)
    table(data$ba5, exclude = NULL)

    # * |++| preterm_all (in months) |++|;
    data$preterm_all_mo <- ifelse((data$i367b %in% "y" & data$i166o %in% "y" & data$fb_day0 == "y" & data$i147o %in% "n" & data$i284o %in% "n") | data$i367c %in% "y", 1, 2)
    table(data$preterm_all_mo, exclude = NULL)

    # Children 1-59m Hieararcy
    data$allexpertdxs <- NA
    data$allexpertdxs[is.na(data$allexpertdxs) & (data$ba5==1 | data$bi5==1)] <- "Intrapartum"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$congmalf2==1] <- "Malformation"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$preterm_all_mo==1] <- "Preterm"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$injury3_slide15_4==1] <- "Injury"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$AIDS5==1 & data$measles4==2 & data$meningitis==2 & data$pertussis==2 & data$malaria251==2 & data$hemfever==2] <- "AIDS"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$measles4==1] <- "Measles"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$meningitis==1] <- "Meningitis/Encephalitis"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$AIDS5==1 & data$diardysn8==1 ] <- "AIDS"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$malnutrition2==1 & data$diardysn8==1] <- "Malnutrition"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$diardysn8==1] <- "Diarrhea/Dysentery"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$pertussis==1] <- "Other infections"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$AIDS5==1 & data$pneumoniafb2daysgr==1] <- "AIDS"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$malnutrition2==1 & data$pneumoniafb2daysgr==1] <- "Malnutrition"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$pneumoniafb2daysgr==1] <- "Pneumonia"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$malaria251==1] <- "Malaria"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$AIDS5==1 & (data$possdiardysn8_4==1 | data$possibleari3==1)] <- "AIDS"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$malnutrition2==1 & (data$possdiardysn8_4==1 | data$possibleari3==1)] <- "Malnutrition"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$possdiardysn8_4==1] <- "Diarrhea/Dysentery"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$possibleari3==1] <- "Pneumonia"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$hemfever==1] <- "Other infections"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$sepsis_nomal251==1] <- "Other infections"
    # data$allexpertdxs[is.na(data$allexpertdxs) & data$malaria_possible==1] <- "Malaria"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$residual_infect_slide15_4==1] <- "Other infections"
    data$allexpertdxs[is.na(data$allexpertdxs) & data$malnutrition1==1] <- "Malnutrition"
    data$allexpertdxs[is.na(data$allexpertdxs)] <- "Unspecified"

  }


  eava_cod <- data[,c("ID","allexpertdxs")]
  names(eava_cod)[names(eava_cod)=="allexpertdxs"] <- "cause"

  return(as.data.frame(eava_cod, stringsAsFactors = FALSE))

}
