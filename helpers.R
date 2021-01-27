library("shiny")
library("dplyr")
library("tidyr") #for drop_na()
library(readxl) 
library("plyr")

# Locate measurement and data Cleaning (LC)
aol_mean <- function(df, measures){
  df1 <- data.frame(df())#after reactive, the df is not the dataframe. so need to add ().
  df1 <- df1[-1,]
  
  df2 <- data.frame(df1) %>%
    dplyr::select(contains(measures)) %>%
    dplyr::mutate(ID = df1$Q65) #Q65 is the participant ID column
  df3 = df2[!duplicated(df2$ID),]#duplicated data, only keep the first
  
  
################################Q37: PANAS################################
  while(measures == "Q37"){
    ########measure info########
    #item numbers#
    ls_num <-  26
    #subscale infos#
    ls_sub = list("Q37" = list("sub1" = list("name" = "Positive Affect", "item" = c(1,3,4,6,11,12,15,17,20,21,22,24,26)),
                               "sub2" = list("name" = "Negative Affect", "item" = c(2,5,7,8,9,10,13,14,16,18,19,23,25))))
    #recode#
    df3[df3=="Very Slightly or Not at All"] <- 1
    df3[df3=="A Little\n"] <- 2 # check out this happens in other measures??? if so, do data clean first
    df3[df3=="Moderately\n"] <- 3
    df3[df3=="Quite a Bit\n"] <- 4
    df3[df3=="Extremely"] <- 5
    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }

    ########reversing: skip
    #######subscales########
    df_final <- df3
    for(i in 1:length(ls_sub[[measures]])){####here,i in 1:length()!!instead of i in length() which only return the last round.
      sub_names = ls_sub[[measures]][[i]]$name
      sub_item = ls_sub[[measures]][[i]]$item

      #get the full name of subscale items
      sub_item_full = c()
      for(j in 1:length(sub_item)){
        col_sub = paste(measures,"_",sub_item[j],sep = "")
        sub_item_full = append(sub_item_full, col_sub)
      }
      #calculate the scores of sub_scale
      sub_sum_IncluNA <- rowSums(df3[,sub_item_full])
      sub_mean_ExcluNA = rowMeans(df3[,sub_item_full], na.rm = TRUE)
      
      df_final <- df_final %>% #save to df_final
        dplyr::mutate(new_col1 = sub_sum_IncluNA) %>%
        dplyr::mutate(new_col2 = sub_mean_ExcluNA)

      sub_sum_IncluNA_name <- paste(sub_names,"_","Sum",sep = "")#NA indicating at least 1 missing data for this participant
      sub_mean_ExcluNA_name <- paste(sub_names,"_","Mean_ExcluNA",sep = "")
      colnames(df_final)[length(colnames(df_final))-1] <- sub_sum_IncluNA_name
      colnames(df_final)[length(colnames(df_final))] <- sub_mean_ExcluNA_name
      
    }
    ########totals########
    sum_all <- rowSums(df3[,1:ls_num])
    mean_all_ExcluNA <- rowMeans(df3[,1:ls_num],na.rm = TRUE)
    df_final <- df_final %>%
      dplyr::mutate(Total_sum = sum_all) %>%
      dplyr::mutate(Total_mean_ExluNA = mean_all_ExcluNA)
    
    return(df_final)
  }
 
################################Q20: CES-D################################
  while(measures == "Q20"){
    ########measure info########
    #item numbers#
    ls_num <-  20
    #subscale infos: skip#
    #recode#
    df3[df3=="None of the time (less than 1 day)"] <- 0
    df3[df3=="A little of the time (1-2 days)"] <- 1
    df3[df3=="A moderate amount of time (3-4 days)"] <- 2
    df3[df3=="Most of the time (5-7 days)"] <- 3
    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }

    ########reversing########
    ls_rev = list("ls_scale" = 3,#3-0,3-1,3-2,3-3. attention: here scores starting from 0 rather than 1
                  "ls_rev_item" = c(4,8,12,16))

    item_rev = ls_rev$ls_rev_item
    item_scale = ls_rev$ls_scale
    for(i in 1:length(item_rev)){
      col_rev = paste(measures,"_",item_rev[i],sep = "")
      df3[,col_rev] = item_scale - df3[,col_rev]
    }
    df_final <- df3
    #######subscales:skip
    ########totals########
    sum_all <- rowSums(df3[,1:ls_num])
    mean_all_ExcluNA <- rowMeans(df3[,1:ls_num],na.rm = TRUE)
    df_final <- df_final %>%
      dplyr::mutate(Total_sum = sum_all) %>%
      dplyr::mutate(Total_mean_ExluNA = mean_all_ExcluNA)
    
    return(df_final)
  }
  
################################Q35: ERQ################################
  while(measures == "Q35"){
    ########measure info########
    #item numbers#
    ls_num <-  10
    #subscale infos#
    ls_sub = list("Q35" = list("sub1" = list("name" = "Cognitive_Reappraisal_facet", "item" = c(1,3,5,7,8,10)),
                               "sub2" = list("name" = "Expressive_Suppression_facet", "item" = c(2,4,6,9))))
    #recode#
    df3[df3=="1 Strongly disagree"] <- 1
    df3[df3=="2\nMostly disagree"] <- 2 # check out this happens in other measures??? if so, do data clean first
    df3[df3=="3\nSomewhat disagree"] <- 3
    df3[df3=="4\nNeutral"] <- 4
    df3[df3=="5\nSomewhat agree"] <- 5
    df3[df3=="6\nMostly agree"] <- 6
    df3[df3=="7\nStrongly agree"] <- 7
    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }
    
    ########reversing: skip
    #######subscales########
    df_final <- df3
    for(i in 1:length(ls_sub[[measures]])){####here,i in 1:length()!!instead of i in length() which only return the last round.
      sub_names = ls_sub[[measures]][[i]]$name
      sub_item = ls_sub[[measures]][[i]]$item
      
      #get the full name of subscale items
      sub_item_full = c()
      for(j in 1:length(sub_item)){
        col_sub = paste(measures,"_",sub_item[j],sep = "")
        sub_item_full = append(sub_item_full, col_sub)
      }
      #calculate the scores of sub_scale
      sub_sum_IncluNA <- rowSums(df3[,sub_item_full])
      sub_mean_ExcluNA = rowMeans(df3[,sub_item_full], na.rm = TRUE)
      
      df_final <- df_final %>% #save to df_final
        dplyr::mutate(new_col1 = sub_sum_IncluNA) %>%
        dplyr::mutate(new_col2 = sub_mean_ExcluNA)
      
      sub_sum_IncluNA_name <- paste(sub_names,"_","Sum",sep = "")#NA indicating at least 1 missing data for this participant
      sub_mean_ExcluNA_name <- paste(sub_names,"_","Mean_ExcluNA",sep = "")
      colnames(df_final)[length(colnames(df_final))-1] <- sub_sum_IncluNA_name
      colnames(df_final)[length(colnames(df_final))] <- sub_mean_ExcluNA_name
    }
    ########totals########
    sum_all <- rowSums(df3[,1:ls_num])
    mean_all_ExcluNA <- rowMeans(df3[,1:ls_num],na.rm = TRUE)
    df_final <- df_final %>%
      dplyr::mutate(Total_sum = sum_all) %>%
      dplyr::mutate(Total_mean_ExluNA = mean_all_ExcluNA)
    
    return(df_final)
  }
  
################################Q35: BFI################################
  while(measures == "Q64"){
    ########measure info########
    #item numbers#
    ls_num <-  44
    #subscale infos#
    ls_sub = list("Q64" = list("sub1" = list("name" = "Extraversion", "item" = c(1,6,11,16,21,26,31,36)),
                               "sub2" = list("name" = "Agreeableness", "item" = c(2,7,12,17,22,27,32,37,42)),
                               "sub3" = list("name" = "Conscientiousness", "item" = c(3,8,13,18,23,28,33,38,43)),
                               "sub4" = list("name" = "Neuroticism", "item" = c(4,9,14,19,24,29,34,39)),
                               "sub5" = list("name" = "Openness", "item" = c(5,10,15,20,25,30,35,40,41,44))))
    #recode#
    df3[df3=="1\nDisagree Strongly\n"] <- 1
    df3[df3=="2\nDisagree a Little\n"] <- 2 # check out this happens in other measures??? if so, do data clean first
    df3[df3=="3\nNeither Agree or Disagree\n"] <- 3
    df3[df3=="4\nAgree a Little\n"] <- 4
    df3[df3=="5\nAgree Strongly\n"] <- 5
    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }
    
    ########reversing########
    ls_rev = list("ls_scale" = 6,#6-1,6-2,6-3,6-4,6-5. attention: here scores starting from 0 rather than 1
                  "ls_rev_item" = c(2,6,8,9,12,18,21,23,24,27,31,34,35,37,41,43))
    
    item_rev = ls_rev$ls_rev_item
    item_scale = ls_rev$ls_scale
    for(i in 1:length(item_rev)){
      col_rev = paste(measures,"_",item_rev[i],sep = "")
      df3[,col_rev] = item_scale - df3[,col_rev]
    }
    df_final <- df3
    #######subscales########
    df_final <- df3
    for(i in 1:length(ls_sub[[measures]])){####here,i in 1:length()!!instead of i in length() which only return the last round.
      sub_names = ls_sub[[measures]][[i]]$name
      sub_item = ls_sub[[measures]][[i]]$item
      
      #get the full name of subscale items
      sub_item_full = c()
      for(j in 1:length(sub_item)){
        col_sub = paste(measures,"_",sub_item[j],sep = "")
        sub_item_full = append(sub_item_full, col_sub)
      }
      #calculate the scores of sub_scale
      sub_sum_IncluNA <- rowSums(df3[,sub_item_full])
      sub_mean_ExcluNA = rowMeans(df3[,sub_item_full], na.rm = TRUE)
      
      df_final <- df_final %>% #save to df_final
        dplyr::mutate(new_col1 = sub_sum_IncluNA) %>%
        dplyr::mutate(new_col2 = sub_mean_ExcluNA)
      
      sub_sum_IncluNA_name <- paste(sub_names,"_","Sum",sep = "")#NA indicating at least 1 missing data for this participant
      sub_mean_ExcluNA_name <- paste(sub_names,"_","Mean_ExcluNA",sep = "")
      colnames(df_final)[length(colnames(df_final))-1] <- sub_sum_IncluNA_name
      colnames(df_final)[length(colnames(df_final))] <- sub_mean_ExcluNA_name
    }
    ########totals########
    sum_all <- rowSums(df3[,1:ls_num])
    mean_all_ExcluNA <- rowMeans(df3[,1:ls_num],na.rm = TRUE)
    df_final <- df_final %>%
      dplyr::mutate(Total_sum = sum_all) %>%
      dplyr::mutate(Total_mean_ExluNA = mean_all_ExcluNA)
    
    return(df_final)
  } 
################################Q67: EQ################################
  while(measures == "Q67"){
    ########measure info########
    #item numbers#
    ls_num <-  32
    #subscale infos#
    ls_scoring = list("scoring_1" = c(6,8,9,12,16,26),#strongly disagree:x2; slightly disagree:x1
                      "scoring_2" = c(2,5,11,13,14,15,17,20,23,24,25,27,28,30,31,32),#strongly agree:x2; slightly agree:x1
                      "scoring_3" = c(29,4,7,21,1,22,18,10,3,19))#x0
    #recoding#
    for(i in 1:length(ls_scoring)){
      sub_item_full = c()
      for(j in 1:length(ls_scoring[[i]])){
        col_sub = paste(measures,"_",ls_scoring[[i]][j],sep = "")
        sub_item_full = append(sub_item_full, col_sub)
      }
      if(i==1){#while is not good if i remains same without changes. it will loop always! so while+break.
        df3[,sub_item_full][df3[,sub_item_full]=="1\nStrongly disagree"] <- 0
        df3[,sub_item_full][df3[,sub_item_full]=="2\nModerately disagree"] <- 0
        df3[,sub_item_full][df3[,sub_item_full]=="3\nNeither agree nor disagree"] <- 0
        df3[,sub_item_full][df3[,sub_item_full]=="4\nModerately agree"] <- 1
        df3[,sub_item_full][df3[,sub_item_full]=="5\nStrongly agree"] <- 2
      } else if(i==2){
        df3[,sub_item_full][df3[,sub_item_full]=="1\nStrongly disagree"] <- 2
        df3[,sub_item_full][df3[,sub_item_full]=="2\nModerately disagree"] <- 1
        df3[,sub_item_full][df3[,sub_item_full]=="3\nNeither agree nor disagree"] <- 0
        df3[,sub_item_full][df3[,sub_item_full]=="4\nModerately agree"] <- 0
        df3[,sub_item_full][df3[,sub_item_full]=="5\nStrongly agree"] <- 0
      } else {
        df3[,sub_item_full][df3[,sub_item_full]=="1\nStrongly disagree"] <- 0
        df3[,sub_item_full][df3[,sub_item_full]=="2\nModerately disagree"] <- 0
        df3[,sub_item_full][df3[,sub_item_full]=="3\nNeither agree nor disagree"] <- 0
        df3[,sub_item_full][df3[,sub_item_full]=="4\nModerately agree"] <- 0
        df3[,sub_item_full][df3[,sub_item_full]=="5\nStrongly agree"] <- 0
      }
      
    }

    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }
    df_final <- df3
    ########reversing: skip########
    #######subscales:skip########
    ########totals########
    sum_all <- rowSums(df3[,1:ls_num])
    mean_all_ExcluNA <- rowMeans(df3[,1:ls_num],na.rm = TRUE)
    df_final <- df_final %>%
      dplyr::mutate(Total_sum = sum_all) %>%
      dplyr::mutate(Total_mean_ExluNA = mean_all_ExcluNA)
    
    return(df_final)
  }
  
################################Q19: AES################################
  while(measures == "Q19"){
    ########measure info########
    #item numbers#
    ls_num <-  16
    #subscale infos#
    ls_sub = list("Q19" = list("sub1" = list("name" = "Cognitive", "item" = c(1,3,4,5,8,11)),
                               "sub2" = list("name" = "Behavior", "item" = c(2,6,9,10,12)),
                               "sub3" = list("name" = "Emotional", "item" = c(7,13)),
                               "sub4" = list("name" = "Other", "item" = c(14,15,16))))
    
    ########reversing########
    ls_rev = list("ls_rev_item" = c(6,10,11))
    item_rev = ls_rev$ls_rev_item
    rev_item_full = c()
    for(j in 1:length(item_rev)){
      col_sub = paste(measures,"_",item_rev[j],sep = "")
      rev_item_full = append(rev_item_full, col_sub)
    }
    df3[,rev_item_full][df3[,rev_item_full]=="Not at all"] <- "A lot"
    df3[,rev_item_full][df3[,rev_item_full]=="Slightly"] <- "Somewhat"
    df3[,rev_item_full][df3[,rev_item_full]=="Somewhat"] <- "Slightly"
    df3[,rev_item_full][df3[,rev_item_full]=="A lot"] <- "Not at all"
    
    ########recoding########
    df3[df3=="Not at all"] <- 1
    df3[df3=="Slightly"] <- 1# check out this happens in other measures??? if so, do data clean first
    df3[df3=="Somewhat"] <- 3
    df3[df3=="A lot"] <- 4
    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }
    df_final <- df3
    #######subscales########
    for(i in 1:length(ls_sub[[measures]])){####here,i in 1:length()!!instead of i in length() which only return the last round.
      sub_names = ls_sub[[measures]][[i]]$name
      sub_item = ls_sub[[measures]][[i]]$item
      
      #get the full name of subscale items
      sub_item_full = c()
      for(j in 1:length(sub_item)){
        col_sub = paste(measures,"_",sub_item[j],sep = "")
        sub_item_full = append(sub_item_full, col_sub)
      }
      #calculate the scores of sub_scale
      sub_sum_IncluNA <- rowSums(df3[,sub_item_full])
      sub_mean_ExcluNA = rowMeans(df3[,sub_item_full], na.rm = TRUE)
      
      df_final <- df_final %>% #save to df_final
        dplyr::mutate(new_col1 = sub_sum_IncluNA) %>%
        dplyr::mutate(new_col2 = sub_mean_ExcluNA)
      
      sub_sum_IncluNA_name <- paste(sub_names,"_","Sum",sep = "")#NA indicating at least 1 missing data for this participant
      sub_mean_ExcluNA_name <- paste(sub_names,"_","Mean_ExcluNA",sep = "")
      colnames(df_final)[length(colnames(df_final))-1] <- sub_sum_IncluNA_name
      colnames(df_final)[length(colnames(df_final))] <- sub_mean_ExcluNA_name
    }
    ########totals########
    sum_all <- rowSums(df3[,1:ls_num])
    mean_all_ExcluNA <- rowMeans(df3[,1:ls_num],na.rm = TRUE)
    df_final <- df_final %>%
      dplyr::mutate(Total_sum = sum_all) %>%
      dplyr::mutate(Total_mean_ExluNA = mean_all_ExcluNA)
    
    return(df_final)
  }  

################################Q36: ITW################################
  while(measures == "Q36"){
    ########measure info########
    #item numbers#
    ls_num <-  5
    #subscale infos:N/A#
    #recode#
    df3[df3=="1\nStrongly agree"] <- 1
    df3[df3=="2\nModerately agree"] <- 2 # check out this happens in other measures??? if so, do data clean first
    df3[df3=="3\nSlightly agree"] <- 3
    df3[df3=="4\nSlightly disagree"] <- 4
    df3[df3=="5\nModerately disagree"] <- 5
    df3[df3=="6\nStrongly disagree"] <- 6
    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }
    
    ########reversing########
    ls_rev = list("ls_scale" = 7,#7-1,7-2,7-3,7-4,7-5,7-6.
                  "ls_rev_item" = c(3,4))
    
    item_rev = ls_rev$ls_rev_item
    item_scale = ls_rev$ls_scale
    for(i in 1:length(item_rev)){
      col_rev = paste(measures,"_",item_rev[i],sep = "")
      df3[,col_rev] = item_scale - df3[,col_rev]
    }
    df_final <- df3
    #######subscales:skip########
    ########totals########
    sum_all <- rowSums(df3[,1:ls_num])
    mean_all_ExcluNA <- rowMeans(df3[,1:ls_num],na.rm = TRUE)
    df_final <- df_final %>%
      dplyr::mutate(Total_sum = sum_all) %>%
      dplyr::mutate(Total_mean_ExluNA = mean_all_ExcluNA)
    
    return(df_final)
  }
################################Q53: WBS################################
  while(measures == "Q53"){
    ########measure info########
    #item numbers#
    ls_num <-  42
    #subscale infos#
    ls_sub = list("Q53" = list("sub1" = list("name" = "Autonomy", "item" = c(1,7,13,19,25,31,37)),
                               "sub2" = list("name" = "Environmental mastery", "item" = c(2,8,14,20,26,32,38)),
                               "sub3" = list("name" = "Personal Growth", "item" = c(3,9,15,21,27,33,39)),
                               "sub4" = list("name" = "Positive Relations", "item" = c(4,10,16,22,28,34,40)),
                               "sub5" = list("name" = "Purpose in life", "item" = c(5,11,17,23,29,35,41)),
                               "sub6" = list("name" = "Self-acceptance", "item" = c(6,12,18,24,30,36,42))))
    #recode#
    df3[df3=="1\nStrongly Disagree\n"] <- 1
    df3[df3=="2\n"] <- 2 # check out this happens in other measures??? if so, do data clean first
    df3[df3=="3\n"] <- 3
    df3[df3=="4\n"] <- 4
    df3[df3=="5\n"] <- 5
    df3[df3=="6\nStrongly Agree\n"] <- 6
    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }
    
    ########reversing########
    ls_rev = list("ls_scale" = 7,#7-1,7-2,7-3,7-4,7-5,7-6. attention: here scores starting from 0 rather than 1
                  "ls_rev_item" = c(3,5,10,13,14,15,16,17,18,19,23,26,27,30,31,32,34,36,39,41))
    
    item_rev = ls_rev$ls_rev_item
    item_scale = ls_rev$ls_scale
    for(i in 1:length(item_rev)){
      col_rev = paste(measures,"_",item_rev[i],sep = "")
      df3[,col_rev] = item_scale - df3[,col_rev]
    }
    df_final <- df3
    #######subscales########
    df_final <- df3
    for(i in 1:length(ls_sub[[measures]])){####here,i in 1:length()!!instead of i in length() which only return the last round.
      sub_names = ls_sub[[measures]][[i]]$name
      sub_item = ls_sub[[measures]][[i]]$item
      
      #get the full name of subscale items
      sub_item_full = c()
      for(j in 1:length(sub_item)){
        col_sub = paste(measures,"_",sub_item[j],sep = "")
        sub_item_full = append(sub_item_full, col_sub)
      }
      #calculate the scores of sub_scale
      sub_sum_IncluNA <- rowSums(df3[,sub_item_full])
      sub_mean_ExcluNA = rowMeans(df3[,sub_item_full], na.rm = TRUE)
      
      df_final <- df_final %>% #save to df_final
        dplyr::mutate(new_col1 = sub_sum_IncluNA) %>%
        dplyr::mutate(new_col2 = sub_mean_ExcluNA)
      
      sub_sum_IncluNA_name <- paste(sub_names,"_","Sum",sep = "")#NA indicating at least 1 missing data for this participant
      sub_mean_ExcluNA_name <- paste(sub_names,"_","Mean_ExcluNA",sep = "")
      colnames(df_final)[length(colnames(df_final))-1] <- sub_sum_IncluNA_name
      colnames(df_final)[length(colnames(df_final))] <- sub_mean_ExcluNA_name
      
    }
    ########totals########
    sum_all <- rowSums(df3[,1:ls_num])
    mean_all_ExcluNA <- rowMeans(df3[,1:ls_num],na.rm = TRUE)
    df_final <- df_final %>%
      dplyr::mutate(Total_sum = sum_all) %>%
      dplyr::mutate(Total_mean_ExluNA = mean_all_ExcluNA)
    
    return(df_final)
  } 
  
################################Q54: STAI################################
  while(measures == "Q54"){
    ########measure info########
    #item numbers#
    ls_num <-  20
    #subscale infos: No#
    #recode#
    df3[df3=="Not at all"] <- 1
    df3[df3=="Moderately so"] <- 2
    df3[df3=="Somewhat"] <- 3
    df3[df3=="Very much so"] <- 4
    
    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }
    
    ########reversing########
    ls_rev = list("ls_scale" = 5,#5-1,5-2,5-3,5-4. attention: here scores starting from 0 rather than 1
                  "ls_rev_item" = c(1,2,5,8,10,11,15,16,19,20))
    
    item_rev = ls_rev$ls_rev_item
    item_scale = ls_rev$ls_scale
    for(i in 1:length(item_rev)){
      col_rev = paste(measures,"_",item_rev[i],sep = "")
      df3[,col_rev] = item_scale - df3[,col_rev]
    }
    df_final <- df3
    #######subscales:skip########
    ########totals########
    sum_all <- rowSums(df3[,1:ls_num])
    mean_all_ExcluNA <- rowMeans(df3[,1:ls_num],na.rm = TRUE)
    df_final <- df_final %>%
      dplyr::mutate(Total_sum = sum_all) %>%
      dplyr::mutate(Total_mean_ExluNA = mean_all_ExcluNA)
    
    return(df_final)
  }
################################Q56: SWLS################################
  while(measures == "Q56"){
    ########measure info########
    #item numbers#
    ls_num <-  5
    #subscale infos: No#
    #recode#
    df3[df3=="1\nStrongly disagree"] <- 1
    df3[df3=="2\nDisagree"] <- 2
    df3[df3=="3\nSlightly disagree"] <- 3
    df3[df3=="4\nNeither agree nor disagree"] <- 4
    df3[df3=="5\nSlightly agree"] <- 5
    df3[df3=="6\nAgree"] <- 6
    df3[df3=="7\nStrongly agree"] <- 7
    
    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }
    
    ########reversing:skip########
    #######subscales:skip########
    ########totals########
    df_final <- df3
    sum_all <- rowSums(df3[,1:ls_num])
    mean_all_ExcluNA <- rowMeans(df3[,1:ls_num],na.rm = TRUE)
    df_final <- df_final %>%
      dplyr::mutate(Total_sum = sum_all) %>%
      dplyr::mutate(Total_mean_ExluNA = mean_all_ExcluNA)

    return(df_final)
  }  
################################Q58: TMMS################################
  while(measures == "Q58"){
    ########measure info########
    #item numbers#
    ls_num <-  48
    #subscale infos#
    ls_sub = list("Q58" = list("sub1" = list("name" = "Attention", "item" = c(4,7,8,15,18,22,29,31,35,38,41,44,46)),
                               "sub2" = list("name" = "Clarify", "item" = c(9,12,19,24,26,28,33,37,42,45,48)),
                               "sub3" = list("name" = "Repair", "item" = c(2,16,17,23,32,43))))
    #recode#
    df3[df3=="Strongly disagree"] <- 1
    df3[df3=="Moderately disagree"] <- 2
    df3[df3=="Neither disagree nor agree"] <- 3
    df3[df3=="Moderately agree"] <- 4
    df3[df3=="Strongly agree"] <- 5

    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }
    ########reversing########
    ls_rev = list("ls_scale" = 6,#6-1,6-2,6-3,6-4,6-5.
                  "ls_rev_item" = c(4,7,8,9,17,19,24,28,29,31,32,37,38,44,46))
    
    item_rev = ls_rev$ls_rev_item
    item_scale = ls_rev$ls_scale
    for(i in 1:length(item_rev)){
      col_rev = paste(measures,"_",item_rev[i],sep = "")
      df3[,col_rev] = item_scale - df3[,col_rev]
    }
    df_final <- df3
    #######subscales########
    for(i in 1:length(ls_sub[[measures]])){####here,i in 1:length()!!instead of i in length() which only return the last round.
      sub_names = ls_sub[[measures]][[i]]$name
      sub_item = ls_sub[[measures]][[i]]$item
      
      #get the full name of subscale items
      sub_item_full = c()
      for(j in 1:length(sub_item)){
        col_sub = paste(measures,"_",sub_item[j],sep = "")
        sub_item_full = append(sub_item_full, col_sub)
      }
      #calculate the scores of sub_scale
      sub_sum_IncluNA <- rowSums(df3[,sub_item_full])
      sub_mean_ExcluNA = rowMeans(df3[,sub_item_full], na.rm = TRUE)
      
      df_final <- df_final %>% #save to df_final
        dplyr::mutate(new_col1 = sub_sum_IncluNA) %>%
        dplyr::mutate(new_col2 = sub_mean_ExcluNA)
      
      sub_sum_IncluNA_name <- paste(sub_names,"_","Sum",sep = "")#NA indicating at least 1 missing data for this participant
      sub_mean_ExcluNA_name <- paste(sub_names,"_","Mean_ExcluNA",sep = "")
      colnames(df_final)[length(colnames(df_final))-1] <- sub_sum_IncluNA_name
      colnames(df_final)[length(colnames(df_final))] <- sub_mean_ExcluNA_name
    }
    ########totals:skip########
    return(df_final)
  }
################################Q59: UCLA Loneliness################################
  while(measures == "Q59"){
    ########measure info########
    #item numbers#
    ls_num <-  20
    #subscale infos: No#
    #recode#
    df3[df3=="1\nI never feel \nthis way\n"] <- 0
    df3[df3=="2\nI rarely feel\nthis way\n"] <- 1
    df3[df3=="3\nI sometimes feel\nthis way\n"] <- 2
    df3[df3=="4\nI often feel\nthis way\n"] <- 3
    
    for(t in 1:ls_num){
      colnam <- colnames(df3[t])
      df3[,colnam] <- as.numeric(df3[,colnam]) #cannot use df3$colnam, here colnam will be identified as a specific variable
    }
    
    ########reversing:skip########
    #######subscales:skip########
    ########totals########
    df_final <- df3
    sum_all <- rowSums(df3[,1:ls_num])
    mean_all_ExcluNA <- rowMeans(df3[,1:ls_num],na.rm = TRUE)
    df_final <- df_final %>%
      dplyr::mutate(Total_sum = sum_all) %>%
      dplyr::mutate(Total_mean_ExluNA = mean_all_ExcluNA)
    
    return(df_final)
  }   
}



 
  
  


