################################################################################
#' Function to calculate the MAGGIC Score (2013)
#'
#' These functions calculate the mortality risk of patients according to
#' the MAGGIC score, which originally uses a lookup table to assign a scoring value
#' to each health parameter.
#' This way, a final score is calculated as a sum, which can be translated to the
#' patient's mortality risk via a second lookup table.
#' The R version of the scoring algorithm was developed using the following sources:
#'
#' Predicting survival in heart failure: a risk score based on
#' 39 372 patients from 30 studies
#' Pocock et al. 2013
#' Eur Heart J. doi: 10.1093/eurheartj/ehs337
#'
#' Figure 2: (HF risk parameters to MAGGIC points mapping)
#' Table 4: (MAGGIC points to 1- / 3- year mortality risk)
#'
#' Links as of 21.02.2021:
#'
#' Full text:
#' https://academic.oup.com/eurheartj/article-lookup/doi/10.1093/eurheartj/ehs337
#'
#' The R function should also reconcile with the score calculator at
#' https://www.mdcalc.com/maggic-risk-calculator-heart-failure
#'
#'
#' @importFrom dplyr "%>%"
#' @param target_param Desired outcome of the function (n year mortality or calculated points). Defaults to "1_year_mort". Possible settings are "1_year_mort", "3_year_mort", "points" (returning the points before mapping to a mortality score), "detail_point_info" returning the string of all single scoring calculations (age, lv_ef, ...) concatenated. Order is "age|lv_ef|gender|sys_bp|bmi|creatinine|nyha|smoking|diabetes|copd|hf_gt_18_months|betablock|acei_arb".
#' @param age Health factor patient age in years.
#' @param lv_ef Health factor LV EF, Percentage.
#' @param sys_bp Health factor systolic blood pressure.
#' @param bmi Health factor body mass index.
#' @param creatinine Health factor creatinine in µmol/L.
#' @param nyha Health factor NYHA score, can be I, II, III, IV.
#' @param gender_m Health factor male gender encoded by m=1, f=0.
#' @param smoking Health factor: Is the patient a smoker? Encoded yes=1, no=0.
#' @param diabetes Health factor diabetes. Encoded yes=1, no=0.
#' @param copd Health factor COPD. Encoded yes=1, no=0.
#' @param hf_gt_18_months Duration in months since HF was first diagnosed is 18 months or more. Encoded yes=1, no=0.
#' @param betablock Health factor medication beta blockers encoded yes=1, no=0. Defaults to NA.
#' @param acei_arb Health factor medication ACEi/ARB encoded yes=1, no=0.
#' @return The mortality probability (0-1) for the selected time window, given the selected model. (Or the the raw point information of the maggic score, depending on the target_param setting.)
#' @references
#' Pocock, S. J., Ariti, C. A., McMurray, J. J. V., Maggioni, A., Køber, L., Squire, I. B., Swedberg, K., Dobson, J., Poppe, K. K., Whalley, G. A., & Doughty, R. N. (2013). Predicting survival in heart failure: A risk score based on 39 372 patients from 30 studies. European Heart Journal, 34(19), 1404–1413.
#' @examples
#' \dontrun{
#'
#' # Load the settings data for the maggic score calculation.
#' # In this case it is the lookup table translating from MAGGIC score to mortality risk.
#' data(maggic_score_settings)
#'
#' # The examples require dplyr package
#' library(dplyr)
#' #############################################################################
#' #Test function alone
#' calc_maggic_score(target_param = "1_year_mort",
#'                  age=68,
#'                  lv_ef = 30,
#'                  gender_m=0,
#'                  sys_bp=119,
#'                  bmi=19,
#'                  creatinine=135,
#'                  nyha="II",
#'                  smoking=1,
#'                  diabetes=0,
#'                  copd=0,
#'                  hf_gt_18_months=0,
#'                  betablock=1,
#'                  acei_arb=0)
#'
#' #create and test a dataframe
#'
#' d_test<-data.frame(age=68,
#'                   lvef = 30,
#'                   gender_m=0,
#'                   sys_bp=119,
#'                   bmi=19,
#'                   creatinine=135,
#'                   nyha="II",
#'                   smoker=1,
#'                   diabetes=0,
#'                   copd=0,
#'                   hf_gt_18_months=0,
#'                   betablock=1,
#'                   acei_arb=0
#'  )
#'
#'
#' d_test %>% mutate(maggic_score=calc_maggic_score(target_param = "1_year_mort",
#'                                                 age = age,
#'                                                 lv_ef = lvef,
#'                                                 sys_bp = sys_bp,
#'                                                 bmi = bmi,
#'                                                 creatinine = creatinine,
#'                                                 nyha = nyha,
#'                                                 gender_m = gender_m,
#'                                                 smoking = smoker,
#'                                                 diabetes = diabetes,
#'                                                 copd = copd,
#'                                                 hf_gt_18_months = hf_gt_18_months,
#'                                                 betablock = betablock,
#'                                                 acei_arb = acei_arb)
#'                                                 )
#'
#' }
#' @export
################################################################################


################################################################################
# Actual function implementation
################################################################################

# The functions requires the dplyr package
#library(dplyr)

########################################
# MAGGIC Score Implementation
################################################################################

################################################################################


calc_maggic_score<-Vectorize(
  function(
    target_param = "1_year_mort",
    age,
    lv_ef,
    sys_bp,
    bmi,
    creatinine,
    nyha,
    gender_m,
    smoking,
    diabetes,
    copd,
    hf_gt_18_months,
    betablock,
    acei_arb
    ){

      ef_score=case_when(
        lv_ef<20 ~ 7,
        between(floor(lv_ef), 20, 24) ~ 6,
        between(floor(lv_ef), 25, 29) ~ 5,
        between(floor(lv_ef), 30, 34) ~ 3,
        between(floor(lv_ef), 35, 39) ~ 2,
        lv_ef>=40 ~ 0
        )

      age_score=case_when(
        age<56 ~ 0,
        between(floor(age), 56,59) & lv_ef<30 ~ 1,
        between(floor(age), 56,59) & between(floor(lv_ef),30,39) ~ 2,
        between(floor(age), 56,59) & lv_ef>=40 ~ 3,
        between(floor(age), 60,64) & lv_ef<30 ~ 2,
        between(floor(age), 60,64) & between(floor(lv_ef),30,39) ~ 4,
        between(floor(age), 60,64) & lv_ef>=40 ~ 5,
        between(floor(age), 65,69) & lv_ef<30 ~ 4,
        between(floor(age), 65,69) & between(floor(lv_ef),30,39) ~ 6,
        between(floor(age), 65,69) & lv_ef>=40 ~ 7,
        between(floor(age), 70,74) & lv_ef<30 ~ 6,
        between(floor(age), 70,74) & between(floor(lv_ef),30,39) ~ 8,
        between(floor(age), 70,74) & lv_ef>=40 ~ 9,
        between(floor(age), 75,79) & lv_ef<30 ~ 8,
        between(floor(age), 75,79) & between(floor(lv_ef),30,39) ~ 10,
        between(floor(age), 75,79) & lv_ef>=40 ~ 12,
        age>=80 & lv_ef<30 ~ 10,
        age>=80 & between(floor(lv_ef),30,39) ~ 13,
        age>=80 & lv_ef>=40 ~ 15
        )

      sysbp_score=case_when(
        sys_bp<110 & lv_ef<30 ~ 5,
        sys_bp<110 & between(floor(lv_ef),30,39) ~ 3,
        sys_bp<110 & lv_ef>=40 ~ 2,

        between(floor(sys_bp), 110,119) & lv_ef<30 ~ 4,
        between(floor(sys_bp), 110,119) & between(floor(lv_ef),30,39) ~ 2,
        between(floor(sys_bp), 110,119) & lv_ef>=40 ~ 1,

        between(floor(sys_bp), 120,129) & lv_ef<30 ~ 3,
        between(floor(sys_bp), 120,129) & between(floor(lv_ef),30,39) ~ 1,
        between(floor(sys_bp), 120,129) & lv_ef>=40 ~ 1,

        between(floor(sys_bp), 130,139) & lv_ef<30 ~ 2,
        between(floor(sys_bp), 130,139) & between(floor(lv_ef),30,39) ~ 1,
        between(floor(sys_bp), 130,139) & lv_ef>=40 ~ 0,

        between(floor(sys_bp), 140,149) & lv_ef<30 ~ 1,
        between(floor(sys_bp), 140,149) & between(floor(lv_ef),30,39) ~ 0,
        between(floor(sys_bp), 140,149) & lv_ef>=40 ~ 0,

        sys_bp>=150 ~ 0
        )

      bmi_score=case_when(
        bmi<15 ~ 6,
        between(floor(bmi), 15, 19) ~ 5,
        between(floor(bmi), 20, 24) ~ 3,
        between(floor(bmi), 25, 29) ~ 2,
        bmi>=30 ~ 0
        )

      creatinine_score=case_when(
        creatinine<90 ~ 0,
        between(floor(creatinine),90,109) ~ 1,
        between(floor(creatinine),110,129) ~ 2,
        between(floor(creatinine),130,149) ~ 3,
        between(floor(creatinine),150,169) ~ 4,
        between(floor(creatinine),170,209) ~ 5,
        between(floor(creatinine),210,249) ~ 6,
        creatinine>=250 ~ 8
        )

      nyha_score=case_when(
        nyha=="I" ~ 0,
        nyha=="II" ~ 2,
        nyha=="III" ~ 6,
        nyha=="IV" ~ 8
        )

      gender_score=gender_m
      smoker_score=smoking
      diabetes_score=diabetes *3 #factor 3
      copd_score=copd *2 #factor 2
      hf_18_score=hf_gt_18_months *2 #factor 2
      not_beta_score=if_else(betablock==0, 3, 0)
      not_ace_arb_score=if_else(acei_arb==0, 1, 0)

  maggic_score <-
    ef_score+
              age_score+
              sysbp_score+
              bmi_score+
              creatinine_score+
              nyha_score+
              gender_score+
              smoker_score+
              diabetes_score+
              copd_score+
              hf_18_score+
              not_beta_score+
              not_ace_arb_score

  single_points<- paste(age_score, ef_score, sysbp_score, bmi_score,
                        creatinine_score, nyha_score, gender_score, smoker_score, diabetes_score, copd_score,
                        hf_18_score, not_beta_score, not_ace_arb_score, sep = "|")

  if(target_param=="points"){
    return(maggic_score)
  }
  else if(target_param=="detail_point_info"){
    return(single_points)
  }
  else if(target_param=="1_year_mort" & !is.na(maggic_score)){
    x1_year_mort<-filter(maggic_mortality_lookup,
                         maggic_score==maggic_points)$X1_year_mortality
    return(x1_year_mort)
  }
  else if(target_param=="3_year_mort" & !is.na(maggic_score)){
    x3_year_mort<-filter(maggic_mortality_lookup,
                         maggic_score==maggic_points)$X3_year_mortality
    return(x3_year_mort)
  }
  else {return(NA_real_)}
}, USE.NAMES = FALSE)

