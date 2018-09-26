##Get working directory
getwd()

##Load Libraries
library(tidyverse)
library(lubridate)

#import files into R
fy2015<-read.csv(file ="PERM_FY2015.csv")
fy2016<-read.csv(file ="PERM_FY2016.csv")
fy2017<-read.csv(file ="PERM_FY2017.csv")
fy2018<-read.csv(file ="PERM_FY2018.csv")

#combine the file into 1
perm<-rbind(fy2015,fy2016,fy2017,fy2018)
rm(fy2015,fy2016,fy2017,fy2018)

#Get uniuqe of case status
unique(perm$case_status)

#Filter by case status
permSub<-perm%>%filter(perm$case_status %in% c('Certified-Expired','Certified','Denied'))

#CASE_STATUS
permSub$case_status[permSub$case_status=="Certified-Expired"]<-"Certified"
permSub$case_status<-as.character(permSub$case_status)
permSub$case_status[permSub$case_status=="Certified"]<-1
permSub$case_status[permSub$case_status=="Denied"]<-0
permSub%>%group_by(permSub$case_status)%>%summarise(tot=n())

#Days to decision
permSub$decision_date <- as.Date(strptime(permSub$decision_date,"%m/%d/%Y %H:%M"))
permSub$case_received_date <- as.Date(strptime(permSub$case_received_date,"%m/%d/%Y %H:%M"))

permSub$days_to_decision<-permSub$decision_date-permSub$case_received_date

#Decison Year
permSub$decision_year<-year(permSub$decision_date)
permSub$received_year<-year(permSub$case_received_date)

#REFILE
permSub$refile<-as.character(permSub$refile)
permSub$refile[permSub$refile=="N"]<-0
permSub$refile[permSub$refile=="Y"]<-1
permSub$refile[permSub$refile==""]<-0

permSub%>%group_by(refile)%>%summarise(tot=n())

#EMPLOYER_NUM_EMPLOYEES

#EMPLOYER_NUM_EMPLOYEES_MISSING
permSub$employer_num_employees_missing<-as.character(permSub$employer_num_employees)
permSub$employer_num_employees_missing[!is.na(permSub$employer_num_employees_missing)] <- 0
permSub$employer_num_employees_missing[is.na(permSub$employer_num_employees_missing)] <- 1
permSub%>%group_by(employer_num_employees_missing)%>%summarise(tot=n())

#EMPLOYER_YR_ESTAB

permSub$employer_yr_estab<-as.factor(as.character(permSub$employer_yr_estab))
permSub$employer_yr_estab_missing<-as.character(permSub$employer_yr_estab)
permSub$employer_yr_estab_missing[!is.na(permSub$employer_yr_estab_missing)] <- 0
permSub$employer_yr_estab_missing[is.na(permSub$employer_yr_estab_missing)] <- 1
permSub%>%group_by(employer_yr_estab_missing)%>%summarise(tot=n())

#FW_OWNERSHIP_INTEREST
permSub%>%group_by(fw_ownership_interest)%>%summarise(tot=n())
permSub$fw_ownership_interest<-as.character(permSub$refile)
permSub$fw_ownership_interest[permSub$fw_ownership_interest=="N"]<-0
permSub$fw_ownership_interest[permSub$fw_ownership_interest=="Y"]<-1
permSub$fw_ownership_interest[permSub$fw_ownership_interest==""]<-0
permSub%>%group_by(fw_ownership_interest)%>%summarise(tot=n())

#AGENT_FLAG
permSub$agent_firm_name<-as.numeric(permSub$agent_firm_name)
permSub$agent_firm_name[ permSub$agent_firm_name == "" ] <- NA
permSub$agent_city[ permSub$agent_city == "" ] <- NA
permSub$agent_state[ permSub$agent_state == "" ] <- NA

permSub$agent_flag<-1
sum(permSub$agent_flag)
permSub$agent_flag[is.na(permSub$agent_firm_name)]<-0
permSub$agent_flag[is.na(permSub$agent_city)]<-0
permSub$agent_flag[is.na(permSub$agent_state)]<-0
sum(permSub$agent_flag)


#PW_LEVEL_9089
unique(permSub$pw_level_9089)
permSub$pw_level_9089<-as.character(permSub$pw_level_9089)
permSub$pw_level_9089[ permSub$pw_level_9089 == "" ] <- "0"
permSub$pw_level_9089[ permSub$pw_level_9089 == "Level I" ] <- "1"
permSub$pw_level_9089[ permSub$pw_level_9089 == "Level II" ] <- "2"
permSub$pw_level_9089[ permSub$pw_level_9089 == "Level III" ] <- "3"
permSub$pw_level_9089[ permSub$pw_level_9089 == "Level IV" ] <- "4"
permSub$pw_level_9089[ permSub$pw_level_9089 == "NULL" ] <- "0"
permSub%>%group_by(pw_level_9089)%>%summarise(tot=n())

#foreign_worker_info_education
permSub$foreign_worker_info_education<-as.character(permSub$foreign_worker_info_education)
permSub%>%group_by(foreign_worker_info_education)%>%summarise(tot=n())
permSub$foreign_worker_info_education[permSub$foreign_worker_info_education == "None" ]        <- "0"
permSub$foreign_worker_info_education[permSub$foreign_worker_info_education == "High School" ] <- "1"
permSub$foreign_worker_info_education[permSub$foreign_worker_info_education == "Associate's" ] <- "2"
permSub$foreign_worker_info_education[permSub$foreign_worker_info_education == "Bachelor's" ]  <- "3"
permSub$foreign_worker_info_education[permSub$foreign_worker_info_education == "Master's" ]    <- "4"
permSub$foreign_worker_info_education[permSub$foreign_worker_info_education == "Doctorate" ]   <- "5"
permSub$foreign_worker_info_education[permSub$foreign_worker_info_education == "Other" ]       <- "6"
permSub$foreign_worker_info_education[permSub$foreign_worker_info_education == "" ]            <- "-1"
permSub%>%group_by(foreign_worker_info_education)%>%summarise(tot=n())

#JOB_INFO_JOB_REQ_NORMAL
permSub$job_info_job_req_normal<-as.character(permSub$job_info_job_req_normal)
unique(permSub$job_info_job_req_normal)
permSub$job_info_job_req_normal[permSub$job_info_job_req_normal=="N"]<-0
permSub$job_info_job_req_normal[permSub$job_info_job_req_normal=="Y"]<-1
permSub$job_info_job_req_normal[permSub$job_info_job_req_normal==""]<-0

#JOB_INFO_FOREIGN_LANG_REQ
permSub$job_info_foreign_lang_req<-as.character(permSub$job_info_foreign_lang_req)
unique(permSub$job_info_foreign_lang_req)
permSub$job_info_foreign_lang_req[permSub$job_info_foreign_lang_req=="N"]<-0
permSub$job_info_foreign_lang_req[permSub$job_info_foreign_lang_req=="Y"]<-1
permSub$job_info_foreign_lang_req[permSub$job_info_foreign_lang_req==""]<-0

#JOB_INFO_COMBO_OCCUPATION
permSub$job_info_combo_occupation<-as.character(permSub$job_info_combo_occupation)
unique(permSub$job_info_combo_occupation)
permSub$job_info_combo_occupation[permSub$job_info_combo_occupation=="N"]<-0
permSub$job_info_combo_occupation[permSub$job_info_combo_occupation=="Y"]<-1
permSub$job_info_combo_occupation[permSub$job_info_combo_occupation==""]<-0

#JI_OFFERED_TO_SEC_J_FW
permSub$ji_offered_to_sec_j_fw<-as.character(permSub$ji_offered_to_sec_j_fw)
unique(permSub$ji_offered_to_sec_j_fw)
permSub$ji_offered_to_sec_j_fw[permSub$ji_offered_to_sec_j_fw=="N"]<-0
permSub$ji_offered_to_sec_j_fw[permSub$ji_offered_to_sec_j_fw=="Y"]<-1
permSub$ji_offered_to_sec_j_fw[permSub$ji_offered_to_sec_j_fw==""]<-0

#JI_FW_LIVE_ON_PREMISES
permSub$ji_fw_live_on_premises<-as.character(permSub$ji_fw_live_on_premises)
unique(permSub$ji_fw_live_on_premises)
permSub$ji_fw_live_on_premises[permSub$ji_fw_live_on_premises=="N"]<-0
permSub$ji_fw_live_on_premises[permSub$ji_fw_live_on_premises=="Y"]<-1
permSub$ji_fw_live_on_premises[permSub$ji_fw_live_on_premises==""]<-0

#JI_LIVE_IN_DOMESTIC_SERVICE
permSub$ji_live_in_domestic_service<-as.character(permSub$ji_live_in_domestic_service)
unique(permSub$ji_live_in_domestic_service)
permSub$ji_live_in_domestic_service[permSub$ji_live_in_domestic_service=="N"]<-0
permSub$ji_live_in_domestic_service[permSub$ji_live_in_domestic_service=="Y"]<-1
permSub$ji_live_in_domestic_service[permSub$ji_live_in_domestic_service==""]<-0

#JI_LIVE_IN_DOM_SVC_CONTRACT
permSub$ji_live_in_dom_svc_contract<-as.character(permSub$ji_live_in_dom_svc_contract)
unique(permSub$ji_live_in_dom_svc_contract)

permSub$ji_live_in_dom_svc_contract[permSub$ji_live_in_domestic_service!=1]<-0
permSub$ji_live_in_dom_svc_contract[permSub$ji_live_in_domestic_service==1&permSub$ji_live_in_dom_svc_contract=="N"]<-'-1'
permSub$ji_live_in_dom_svc_contract[permSub$ji_live_in_domestic_service==1&permSub$ji_live_in_dom_svc_contract=="Y"]<-'1'
permSub$ji_live_in_dom_svc_contract[permSub$ji_live_in_dom_svc_contract=="A"]<-0
permSub%>%group_by(ji_live_in_dom_svc_contract)%>%summarise(tot=n())

#RECR_INFO_PROFESSIONAL_OCC
permSub$recr_info_professional_occ<-as.character(permSub$recr_info_professional_occ)
unique(permSub$recr_info_professional_occ)
permSub$recr_info_professional_occ[permSub$recr_info_professional_occ=="N"]<-0
permSub$recr_info_professional_occ[permSub$recr_info_professional_occ=="Y"]<-1
permSub$recr_info_professional_occ[permSub$recr_info_professional_occ==""]<-0

#recr_info_coll_univ_teacher
permSub$recr_info_coll_univ_teacher<-as.character(permSub$recr_info_coll_univ_teacher)
unique(permSub$recr_info_coll_univ_teacher)
permSub$recr_info_coll_univ_teacher[permSub$recr_info_coll_univ_teacher=="N"]<-0
permSub$recr_info_coll_univ_teacher[permSub$recr_info_coll_univ_teacher=="Y"]<-1
permSub$recr_info_coll_univ_teacher[permSub$recr_info_coll_univ_teacher==""]<-0

#recr_info_coll_teach_comp_proc
permSub$recr_info_coll_teach_comp_proc<-as.character(permSub$recr_info_coll_teach_comp_proc)
unique(permSub$recr_info_coll_teach_comp_proc)
permSub%>%group_by(recr_info_coll_univ_teacher,recr_info_coll_teach_comp_proc)%>%summarise(tot=n())

permSub$recr_info_coll_teach_comp_proc[permSub$recr_info_coll_univ_teacher!=1]<-0
permSub$recr_info_coll_teach_comp_proc[permSub$recr_info_coll_univ_teacher==1&permSub$recr_info_coll_teach_comp_proc=="N"]<-'-1'
permSub$recr_info_coll_teach_comp_proc[permSub$recr_info_coll_univ_teacher==1&permSub$recr_info_coll_teach_comp_proc=="Y"]<-'1'
permSub$recr_info_coll_teach_comp_proc[permSub$recr_info_coll_teach_comp_proc==""]<-0
permSub%>%group_by(recr_info_coll_teach_comp_proc)%>%summarise(tot=n())


#ri_coll_tch_basic_process
permSub$ri_coll_tch_basic_process<-as.character(permSub$ri_coll_tch_basic_process)
unique(permSub$ri_coll_tch_basic_process)
permSub%>%group_by(recr_info_coll_univ_teacher,ri_coll_tch_basic_process)%>%summarise(tot=n())

permSub$ri_coll_tch_basic_process[permSub$recr_info_coll_univ_teacher!=1]<-0
permSub$ri_coll_tch_basic_process[permSub$recr_info_coll_univ_teacher==1&permSub$ri_coll_tch_basic_process=="N"]<-'-1'
permSub$ri_coll_tch_basic_process[permSub$recr_info_coll_univ_teacher==1&permSub$ri_coll_tch_basic_process=="Y"]<-'1'
permSub$ri_coll_tch_basic_process[permSub$ri_coll_tch_basic_process==""]<-0
permSub%>%group_by(ri_coll_tch_basic_process)%>%summarise(tot=n())

#Teacher Ad
permSub$ri_coll_teach_pro_jnl<-as.character(permSub$ri_coll_teach_pro_jnl)
permSub%>%group_by(ri_coll_teach_pro_jnl)%>%summarise(tot=n())
permSub$teacher_ad<-1
permSub$teacher_ad[permSub$ri_coll_teach_pro_jnl==""]<-0
permSub%>%group_by(teacher_ad)%>%summarise(tot=n())

#ad_30_day
permSub$recr_info_swa_job_order_end <- as.Date(strptime(permSub$recr_info_swa_job_order_end,"%m/%d/%Y"))
permSub$recr_info_swa_job_order_start <- as.Date(strptime(permSub$recr_info_swa_job_order_start,"%m/%d/%Y"))

permSub$adRun<-permSub$recr_info_swa_job_order_end-permSub$recr_info_swa_job_order_start
permSub$ad_30_day[permSub$adRun>=30]<-1
permSub$ad_30_day[is.na(permSub$adRun)]<-0
permSub$ad_30_day[permSub$adRun<30]<-0
permSub$ad_30_day[permSub$teacher_ad==1]<-1
permSub$adRun<-NULL

#recr_info_sunday_newspaper
permSub%>%group_by(recr_info_sunday_newspaper)%>%summarise(tot=n())
permSub$recr_info_sunday_newspaper<-as.character(permSub$recr_info_sunday_newspaper)
permSub$recr_info_sunday_newspaper[permSub$recr_info_sunday_newspaper=="Y"]<-1
permSub$recr_info_sunday_newspaper[permSub$recr_info_sunday_newspaper=="N"]<-0
permSub$recr_info_sunday_newspaper[permSub$recr_info_sunday_newspaper==""]<-0
permSub$recr_info_sunday_newspaper[permSub$teacher_ad==1]<-1
permSub%>%group_by(recr_info_sunday_newspaper)%>%summarise(tot=n())

#RI_2ND_AD_NEWSPAPER_OR_JOURNAL
permSub%>%group_by(ri_2nd_ad_newspaper_or_journal)%>%summarise(tot=n())
permSub$ri_2nd_ad_newspaper_or_journal<-as.character(permSub$ri_2nd_ad_newspaper_or_journal)
permSub$ri_2nd_ad_newspaper_or_journal[permSub$ri_2nd_ad_newspaper_or_journal=="Y"]<-1
permSub$ri_2nd_ad_newspaper_or_journal[permSub$ri_2nd_ad_newspaper_or_journal=="N"]<-0
permSub$ri_2nd_ad_newspaper_or_journal[permSub$ri_2nd_ad_newspaper_or_journal==""]<-0
permSub$ri_2nd_ad_newspaper_or_journal[permSub$teacher_ad==1]<-1
permSub%>%group_by(ri_2nd_ad_newspaper_or_journal)%>%summarise(tot=n())

#more_than_3ads
permSub$recr_info_job_fair_from=as.character(permSub$recr_info_job_fair_from)
permSub$recr_info_on_campus_recr_from=as.character(permSub$recr_info_on_campus_recr_from)
permSub$recr_info_pro_org_advert_from=as.character(permSub$recr_info_pro_org_advert_from)
permSub$recr_info_radio_tv_ad_from=as.character(permSub$recr_info_radio_tv_ad_from)
permSub$ri_campus_placement_from=as.character(permSub$ri_campus_placement_from)
permSub$ri_employee_referral_prog_from=as.character(permSub$ri_employee_referral_prog_from)
permSub$ri_employer_web_post_from=as.character(permSub$ri_employer_web_post_from)
permSub$ri_job_search_website_from=as.character(permSub$ri_job_search_website_from)
permSub$ri_local_ethnic_paper_from=as.character(permSub$ri_local_ethnic_paper_from)
permSub$ri_pvt_employment_firm_from=as.character(permSub$ri_pvt_employment_firm_from)

permSub$recr_info_job_fair_from[permSub$recr_info_job_fair_from!=""]<-1
permSub$recr_info_on_campus_recr_from[permSub$recr_info_on_campus_recr_from!=""]<-1
permSub$recr_info_pro_org_advert_from[permSub$recr_info_pro_org_advert_from!=""]<-1
permSub$recr_info_radio_tv_ad_from[permSub$recr_info_radio_tv_ad_from!=""]<-1
permSub$ri_campus_placement_from[permSub$ri_campus_placement_from!=""]<-1
permSub$ri_employee_referral_prog_from[permSub$ri_employee_referral_prog_from!=""]<-1
permSub$ri_employer_web_post_from[permSub$ri_employer_web_post_from!=""]<-1
permSub$ri_job_search_website_from[permSub$ri_job_search_website_from!=""]<-1
permSub$ri_local_ethnic_paper_from[permSub$ri_local_ethnic_paper_from!=""]<-1
permSub$ri_pvt_employment_firm_from[permSub$ri_pvt_employment_firm_from!=""]<-1
permSub$recr_info_job_fair_from[permSub$recr_info_job_fair_from==""]<-0
permSub$recr_info_on_campus_recr_from[permSub$recr_info_on_campus_recr_from==""]<-0
permSub$recr_info_pro_org_advert_from[permSub$recr_info_pro_org_advert_from==""]<-0
permSub$recr_info_radio_tv_ad_from[permSub$recr_info_radio_tv_ad_from==""]<-0
permSub$ri_campus_placement_from[permSub$ri_campus_placement_from==""]<-0
permSub$ri_employee_referral_prog_from[permSub$ri_employee_referral_prog_from==""]<-0
permSub$ri_employer_web_post_from[permSub$ri_employer_web_post_from==""]<-0
permSub$ri_job_search_website_from[permSub$ri_job_search_website_from==""]<-0
permSub$ri_local_ethnic_paper_from[permSub$ri_local_ethnic_paper_from==""]<-0
permSub$ri_pvt_employment_firm_from[permSub$ri_pvt_employment_firm_from==""]<-0



permSub$adsRun<-as.numeric(permSub$recr_info_job_fair_from)+
  as.numeric(permSub$recr_info_on_campus_recr_from)+
  as.numeric(permSub$recr_info_pro_org_advert_from)+
  as.numeric(permSub$recr_info_radio_tv_ad_from)+
  as.numeric(permSub$ri_campus_placement_from)+
  as.numeric(permSub$ri_employee_referral_prog_from)+
  as.numeric(permSub$ri_employer_web_post_from)+
  as.numeric(permSub$ri_job_search_website_from)+
  as.numeric(permSub$ri_local_ethnic_paper_from)+
  as.numeric(permSub$ri_pvt_employment_firm_from)

permSub$more_than_3ads[permSub$adsRun>=3]=1
permSub$more_than_3ads[permSub$adsRun<3]=0
permSub$more_than_3ads[permSub$teacher_ad==1]<-1
permSub$adsRun<-NULL

#recr_info_employer_rec_payment
permSub$recr_info_employer_rec_payment<-as.character(permSub$recr_info_employer_rec_payment)
permSub%>%group_by(recr_info_employer_rec_payment)%>%summarise(tot=n())
unique(permSub$recr_info_employer_rec_payment)
permSub$recr_info_employer_rec_payment[permSub$recr_info_employer_rec_payment=="N"]<-0
permSub$recr_info_employer_rec_payment[permSub$recr_info_employer_rec_payment=="Y"]<-1
permSub$recr_info_employer_rec_payment[permSub$recr_info_employer_rec_payment==""]<-0
permSub%>%group_by(recr_info_employer_rec_payment)%>%summarise(tot=n())

#recr_info_barg_rep_notified
permSub%>%group_by(recr_info_barg_rep_notified)%>%summarise(tot=n())
permSub$recr_info_barg_rep_notified<-as.character(permSub$recr_info_barg_rep_notified)
permSub$recr_info_barg_rep_notified[permSub$recr_info_barg_rep_notified=="Y"]<-1
permSub$recr_info_barg_rep_notified[permSub$recr_info_barg_rep_notified=="A"]<-0
permSub$recr_info_barg_rep_notified[permSub$recr_info_barg_rep_notified=="N"]<-"-1"
permSub$recr_info_barg_rep_notified[permSub$recr_info_barg_rep_notified==""]<-"0"
permSub%>%group_by(recr_info_barg_rep_notified)%>%summarise(tot=n())

#ri_posted_notice_at_worksite
permSub%>%group_by(ri_posted_notice_at_worksite)%>%summarise(tot=n())
permSub$ri_posted_notice_at_worksite<-as.character(permSub$ri_posted_notice_at_worksite)
permSub$ri_posted_notice_at_worksite[permSub$ri_posted_notice_at_worksite=="Y"]<-1
permSub$ri_posted_notice_at_worksite[permSub$ri_posted_notice_at_worksite=="A"]<-0
permSub$ri_posted_notice_at_worksite[permSub$ri_posted_notice_at_worksite=="N"]<-"-1"
permSub$ri_posted_notice_at_worksite[permSub$ri_posted_notice_at_worksite==""]<-"0"
permSub%>%group_by(ri_posted_notice_at_worksite)%>%summarise(tot=n())

#ri_layoff_in_past_six_months
permSub%>%group_by(ri_layoff_in_past_six_months)%>%summarise(tot=n())
permSub$ri_layoff_in_past_six_months<-as.character(permSub$ri_layoff_in_past_six_months)
permSub$ri_layoff_in_past_six_months[permSub$ri_layoff_in_past_six_months=="Y"]<-"1"
permSub$ri_layoff_in_past_six_months[permSub$ri_layoff_in_past_six_months=="N"]<-"0"
permSub$ri_layoff_in_past_six_months[permSub$ri_layoff_in_past_six_months==""]<-"0"
permSub%>%group_by(ri_layoff_in_past_six_months)%>%summarise(tot=n())

#ri_us_workers_considered
permSub%>%group_by(ri_us_workers_considered)%>%summarise(tot=n())
permSub$ri_us_workers_considered<-as.character(permSub$ri_us_workers_considered)
permSub$ri_us_workers_considered[permSub$ri_us_workers_considered=="Y"]<-1
permSub$ri_us_workers_considered[permSub$ri_us_workers_considered=="A"]<-0
permSub$ri_us_workers_considered[permSub$ri_us_workers_considered=="N"]<-"-1"
permSub$ri_us_workers_considered[permSub$ri_us_workers_considered==""]<-"0"
permSub%>%group_by(ri_us_workers_considered)%>%summarise(tot=n())

#job_info_education
permSub$job_info_education<-as.character(permSub$job_info_education)
permSub%>%group_by(job_info_education)%>%summarise(tot=n())
permSub$job_info_education[permSub$job_info_education == "None" ]        <- "0"
permSub$job_info_education[permSub$job_info_education == "High School" ] <- "1"
permSub$job_info_education[permSub$job_info_education == "Associate's" ] <- "2"
permSub$job_info_education[permSub$job_info_education == "Bachelor's" ]  <- "3"
permSub$job_info_education[permSub$job_info_education == "Master's" ]    <- "4"
permSub$job_info_education[permSub$job_info_education == "Doctorate" ]   <- "5"
permSub$job_info_education[permSub$job_info_education == "Other" ]       <- "6"
permSub$job_info_education[permSub$job_info_education == "" ]            <- "-1"
permSub%>%group_by(job_info_education)%>%summarise(tot=n())

#fw_info_yr_rel_edu_completed
permSub$fw_info_yr_rel_edu_completed[is.na(permSub$fw_info_yr_rel_edu_completed)]<-0


#fw_info_training_comp
permSub%>%group_by(fw_info_training_comp)%>%summarise(tot=n())
permSub$fw_info_training_comp<-as.character(permSub$fw_info_training_comp)
permSub$fw_info_training_comp[permSub$fw_info_training_comp=="Y"]<-"1"
permSub$fw_info_training_comp[permSub$fw_info_training_comp=="N"]<-"-1"
permSub$fw_info_training_comp[permSub$fw_info_training_comp=="A"]<-"0"
permSub$fw_info_training_comp[permSub$fw_info_training_comp==""]<-"0"
permSub%>%group_by(fw_info_training_comp)%>%summarise(tot=n())

#fw_info_req_experience
permSub%>%group_by(fw_info_req_experience)%>%summarise(tot=n())
permSub$fw_info_req_experience<-as.character(permSub$fw_info_req_experience)
permSub$fw_info_req_experience[permSub$fw_info_req_experience=="Y"]<-"1"
permSub$fw_info_req_experience[permSub$fw_info_req_experience=="N"]<-"-1"
permSub$fw_info_req_experience[permSub$fw_info_req_experience=="A"]<-"0"
permSub$fw_info_req_experience[permSub$fw_info_req_experience==""]<-"0"
permSub%>%group_by(fw_info_req_experience)%>%summarise(tot=n())

#fw_info_alt_edu_experience
permSub%>%group_by(fw_info_alt_edu_experience)%>%summarise(tot=n())
permSub$fw_info_alt_edu_experience<-as.character(permSub$fw_info_alt_edu_experience)
permSub$fw_info_alt_edu_experience[permSub$fw_info_alt_edu_experience=="Y"]<-"1"
permSub$fw_info_alt_edu_experience[permSub$fw_info_alt_edu_experience=="N"]<-"-1"
permSub$fw_info_alt_edu_experience[permSub$fw_info_alt_edu_experience=="A"]<-"0"
permSub$fw_info_alt_edu_experience[permSub$fw_info_alt_edu_experience==""]<-"0"
permSub%>%group_by(fw_info_alt_edu_experience)%>%summarise(tot=n())

#fw_info_rel_occup_exp
permSub%>%group_by(fw_info_rel_occup_exp)%>%summarise(tot=n())
permSub$fw_info_rel_occup_exp<-as.character(permSub$fw_info_rel_occup_exp)
permSub$fw_info_rel_occup_exp[permSub$fw_info_rel_occup_exp=="Y"]<-"1"
permSub$fw_info_rel_occup_exp[permSub$fw_info_rel_occup_exp=="N"]<-"-1"
permSub$fw_info_rel_occup_exp[permSub$fw_info_rel_occup_exp=="A"]<-"0"
permSub$fw_info_rel_occup_exp[permSub$fw_info_rel_occup_exp==""]<-"0"
permSub%>%group_by(fw_info_rel_occup_exp)%>%summarise(tot=n())

permSub$country_of_citizenship[is.na(country_of_citizenship)]<-"-1"
permSub$country_of_citizenship[is.na(fw_info_birth_country)]<-"-1"
permSub$country_of_citizenship[is.na(class_of_admission)]<-"-1"


permFinal<-permSub[,c("case_status",
                      "days_to_decision",
                      "decision_year",
                      "received_year",
                      "refile",
                      "employer_num_employees",
                      "employer_num_employees_missing",
                      "employer_yr_estab",
                      "employer_yr_estab_missing",
                      "fw_ownership_interest",
                      "agent_flag",
                      "pw_level_9089",
                      "job_info_education",
                      "job_info_job_req_normal",
                      "job_info_foreign_lang_req",
                      "job_info_combo_occupation",
                      "ji_offered_to_sec_j_fw",
                      "ji_fw_live_on_premises",
                      "ji_live_in_domestic_service",
                      "ji_live_in_dom_svc_contract",
                      "recr_info_professional_occ",
                      "recr_info_coll_univ_teacher",
                      "recr_info_coll_teach_comp_proc",
                      "ri_coll_tch_basic_process",
                      "teacher_ad",
                      "ad_30_day",
                      "recr_info_sunday_newspaper",
                      "ri_2nd_ad_newspaper_or_journal",
                      "more_than_3ads",
                      "recr_info_employer_rec_payment",
                      "recr_info_barg_rep_notified",
                      "ri_posted_notice_at_worksite",
                      "ri_layoff_in_past_six_months",
                      "ri_us_workers_considered",
                      "country_of_citizenship",
                      "fw_info_birth_country",
                      "class_of_admission",
                      "foreign_worker_info_education",
                      "fw_info_yr_rel_edu_completed",
                      "fw_info_training_comp",
                      "fw_info_req_experience",
                      "fw_info_alt_edu_experience",
                      "fw_info_rel_occup_exp")]


rm(perm,permSub)

write.csv(permFinal,"permFinal.csv",row.names = F)

