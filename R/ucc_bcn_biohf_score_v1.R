################################################################################
#' Function to calculate the Barcelona BioHF Score, Version 1 (2014)
#'
#' Function to calculate the mortality risk of patients according to
#' the Barcelona Bio-Heart Failure Risk Calculator (BCN Bio-HF Calculator)
#' which was published in two different versions.
#'
#' This function calculates the score according to the version 1,
#' published in 2014.
#' Using a set of risk factors this function returns a score between 0 and 1
#' indicating the mortality risk in the predefined timeframe.
#' 8 different models can be used, depending on which of the risk factors
#' can be provided.
#' The algorithm was derived from the following documents of the original
#' publication:
#'
#' Links as of 30.03.2021:
#'
#' File S1:
#'
#' https://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0085466.s002
#'
#' Full article:
#'
#' https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0085466
#'
#' The original web tool published by the authors is no longer publicly
#' available for reconciliation.
#'
#' The second version of the scoring function published in 2018 can be run by using `calc_bcn_biohf_v2()`
#'
#' @importFrom dplyr "%>%"
#' @param target_param Desired outcome of the function (n year mortality). Defaults to "1_year_mort".
#' @param selected_model Model to use for calculation. Has to be set according to available parameters. Defaults to "model1".
#' @param age Health factor patient age in years. Defaults to NA.
#' @param gender_f Health factor female gender encoded by f=1, m=0. Defaults to NA.
#' @param nyha Health factor NYHA score, can be I, II, III, IV. Defaults to NA.
#' @param lv_ef Health factor LV EF, Percentage. Defaults to NA.
#' @param sodium Health factor sodium concentration in mmol/L. Defaults to NA.
#' @param egfr Health factor eGFR in ml/min/m2. Defaults to NA.
#' @param hb Health factor hemoglobin in g/dl. Defaults to NA.
#' @param furosemide1 Health factor medication Furosemide equivalent 0-40 mg/day encoded yes=1, no=0. Defaults to 0.
#' @param furosemide2 Health factor medication Furosemide equivalent >40 mg/day encoded yes=1, no=0. Defaults to 0.
#' @param statin Health factor medication statin encoded yes=1, no=0. Defaults to NA.
#' @param acei_arb Health factor medication ACEi/ARB encoded yes=1, no=0. Defaults to NA.
#' @param betablock Health factor medication beta blockers encoded yes=1, no=0. Defaults to NA.
#' @param ntprobnp Health factor nt-proBNP. Defaults to NA.
#' @param hstnt Health factor high sensitive Troponin T. Defaults to NA.
#' @param st2 Health factor ST2. Defaults to NA.
#' @return The mortality probability (0-1) for the selected time window, given the selected model.
#' @references
#' Lupón, J., De Antonio, M., Vila, J., Peñafiel, J., Galán, A., Zamora, E., Urrutia, A., & Bayes-Genis, A. (2014). Development of a novel heart failure risk tool: The Barcelona bio-heart failure risk calculator (BCN bio-HF calculator). PLoS ONE. https://doi.org/10.1371/journal.pone.0085466
#' @examples
#' \dontrun{
#'
#' # Load the settings data for the BCN BioHF score calculation.
#' # In this case these are tables with the necessary beta values and the min, max and median of the training population to replace missing values and values out of range.
#' data(bcn_hf_score_settings)
#'
#' # The examples require dplyr package
#' library(dplyr)
#' #############################################################################
#' # Examples BCN Bio HF V1
#' # try out the function on a single patient without using it on a dataframe
#'
#' calc_bcn_biohf_v1(target_param = "3_year_mort",
#'                           selected_model="model1",
#'                           age=68,
#'                           gender_f=0,
#'                           nyha="III",
#'                           lv_ef=30,
#'                           sodium=130,
#'                           egfr=45,
#'                           hb=12,
#'                           furosemide2=1,
#'                           statin=1,
#'                           acei_arb=1,
#'                           betablock=1
#'
#' )
#'
#'
#'
#' # Manual calculation fo the score should result in the same number:
#' 1-0.802**exp((-7.51026)-(-8.922))
#' # The formula in the supplement is actually rounded, leading to a slightly
#' # different result
#' 1-0.802**exp((-7.510)-(-8.922))
#'
#'
#' # create and test a dataframe, with 4 patients:
#' # 1. Perfect patient, all values that are necessary
#' # 2. Patient missing a value, that can be imputed with median
#' # 3. Patient with the value that has been imputed median, expect same result
#' # 4. Patient with too high value, should be imputed with 99th percentile
#' # 5. Patient with the value that has been imputed 99th, expect same result
#'
#' d_test<-data.frame( age=rep(68, 5),
#'                     gender_f=rep(0, 5),
#'                     nyha=rep("III", 5),
#'                     lv_ef=rep(30, 5),
#'                     sodium=rep(130, 5),
#'                     egfr=c(45,  NA, 42.4, 300, 110), #This one should be imputed
#'                     hb=rep(12, 5),
#'                     furosemide2=rep(1, 5),
#'                     statin=rep(1, 5),
#'                     acei_arb=rep(1, 5),
#'                     betablock=rep(1, 5)
#' )
#'
#' # The function can be run using dplyr mutate and piping (%>%) instead of
#' # using it on a single patient
#'
#' d_test %>% mutate(bcm_score=calc_bcn_biohf_v1(target_param = "3_year_mort",
#'                                               age = age,
#'                                               gender_f = gender_f,
#'                                               nyha = nyha,
#'                                               lv_ef = lv_ef,
#'                                               sodium = sodium,
#'                                               egfr = egfr,
#'                                               hb = hb,
#'                                               furosemide2 = furosemide2,
#'                                               statin = statin,
#'                                               acei_arb = acei_arb,
#'                                               betablock = betablock
#' )
#' )
#'
#'
#' # Create another dataframe, a mandatory value is missing
#' # 4. Patient with a value missing that cannot be imputed, should give a warning
#'
#' d_test2<-data.frame( # age=68,
#'   gender_f=0,
#'   nyha="III",
#'   lv_ef=30,
#'   sodium=130,
#'   egfr=45,
#'   hb=12,
#'   furosemide2=1,
#'   statin=1,
#'   acei_arb=1,
#'   betablock=1
#' )
#'
#'
#' d_test2 %>% mutate(bcm_score=calc_bcn_biohf_v1(target_param = "3_year_mort",
#'                                                #age = age,
#'                                                gender_f = gender_f,
#'                                                nyha = nyha,
#'                                                lv_ef = lv_ef,
#'                                                sodium = sodium,
#'                                                egfr = egfr,
#'                                                hb = hb,
#'                                                furosemide2 = furosemide2,
#'                                                statin = statin,
#'                                                acei_arb = acei_arb,
#'                                                betablock = betablock
#' )
#' )
#'
#'
#' }
#' @export calc_bcn_biohf_v1
################################################################################


################################################################################
# Actual function implementation
################################################################################

#As described in the Supplement, page 2:
#Model 1 (Clinical) = Age, Female, NYHA functional class (as I-II vs III-IV), LVEF (as ≥ vs < 45%), eGFR (estimated glomerular filtration rate), Sodium,
#Hemoglobin, Furosemide equivalent doses treatment (as 0, ≤ 40 mg/day and > 40 mg/day), ACEI or ARB treatment, β-blocker treatment, statin treatment.


# The functions requires the dplyr package
#library(dplyr)

########################################
#Barcelona Bio HF Score Version 1 (2014)

calc_bcn_biohf_v1<-Vectorize(
  function(
    target_param = "1_year_mort",
    selected_model="model1",
    age=NA,
    gender_f=NA,
    nyha=NA,
    lv_ef=NA,
    sodium=NA,
    egfr=NA,
    hb=NA,
    furosemide1=0,
    furosemide2=0,
    statin=NA,
    acei_arb=NA,
    betablock=NA,
    ntprobnp=NA,
    hstnt=NA,
    st2=NA

  ){#Set the base estimate required for the formula according to the
  # 1-year, 2-year or 3-year mortality that the user selected
  if(target_param=="1_year_mort"){surv_estimate<-0.942}
  else if(target_param=="2_year_mort"){surv_estimate<-0.875}
  else if(target_param=="3_year_mort"){surv_estimate<-0.802}

  #Set the sum product required by the formular according to the model set by
  #the user
  sum_product<-d_coefficientsV1[["sum_product", selected_model]]


  #Check missing values/ min/ max according to the supplement, replace if necessary
  #Define function for the replacement
  check_values<-function(patient_parameter, parameter_name){
    #Extract min max and median
    lower_limit<-d_minmaxmedianV1[parameter_name, "lower_limit"]
    upper_limit<-d_minmaxmedianV1[parameter_name, "upper_limit"]
    median_impute<-d_minmaxmedianV1[parameter_name, "median_impute"]

    #Decide if value is in boundries or should be replaced
    #..if value is not provided, set it to the median (if available)
    patient_parameter = dplyr::case_when(is.na(patient_parameter) ~ median_impute,
                                  #..if value smaller than 1st percentile
                                  #..set to 1st percentile
                                  patient_parameter<lower_limit ~ lower_limit,
                                  #..if value larger than 99th percentile
                                  #..set to 99th percentile
                                  patient_parameter>upper_limit ~ upper_limit,
                                  #Any other case, use the value unchanged
                                  TRUE ~ patient_parameter)
    return(patient_parameter)


  }

  #Do actual check and replace
  lv_ef <-check_values(lv_ef, "lv_ef")
  sodium <-check_values(sodium, "sodium")
  egfr <-check_values(egfr, "egfr")
  hb <-check_values(hb, "hb")

  #Convert parameters to fit into the formula
  nyha = dplyr::case_when(nyha=="I" ~ 0,
                   nyha=="II" ~ 0,
                   nyha=="III" ~ 1,
                   nyha=="IV" ~ 1
                   )
  lv_ef = dplyr::case_when(lv_ef>=45 ~ 1,
                    lv_ef<45 ~ 0)



  #Define a function to calculate the product of a patients parameter
  #and the model coefficient, but only if the model supports that coefficient
  vector_product<-function(patient_parameter, parameter_name){
    #extract the coefficient for this parameter, for the model specified
    coeff<-d_coefficientsV1[[parameter_name, selected_model]]
    #if this model does not support this parameter, but the user supplied it
    #return NA

    if(is.na(coeff) & !is.na(patient_parameter)){
      warning(paste('Parameter "', parameter_name, '" was set for the calculation but the model does not support it!', sep="" ))
      return(NA_real_)
    }
    else if(is.na(coeff) & is.na(patient_parameter)){
      return(0.0)
    }
    else if(!is.na(coeff) & is.na(patient_parameter)){
      warning(paste('Parameter "', parameter_name, '" was not provided, but is required in the model!', sep="" ))
      return(NA_real_)
    }
    else {
      return(patient_parameter * coeff)}
  }

  #Make sure that first value is spelled in a way like the function input:
  # e.g.: hstnt
  # ..but second value is spelled like it is in the beta value lookup table:
  # e.g.: log_hstnt or log_hstnt_square, which use the same function input,
  # but two different beta values to multiply!
  vector_multiplications <- vector_product(age, "age") +
    vector_product(gender_f, "gender_f") +
    vector_product(nyha, "nyha") +
    vector_product(lv_ef, "lv_ef") +
    vector_product(sodium, "sodium") +
    vector_product(egfr, "egfr") +
    vector_product(hb, "hb") +
    vector_product(furosemide1, "furosemide1") +
    vector_product(furosemide2, "furosemide2") +
    vector_product(statin, "statin") +
    vector_product(acei_arb, "acei_arb") +
    vector_product(betablock, "betablock") +
    vector_product(log(ntprobnp), "log_ntprobnp") +
    vector_product(log(hstnt), "log_hstnt") +
    vector_product(log(hstnt)**2, "log_hstnt_square") +
    vector_product(st2/10, "st2_div_10") +
    vector_product((st2/10)**2, "st2_div_10_square")


  mortality_score<- 1-surv_estimate**exp(vector_multiplications -sum_product)

  return(mortality_score)

}, USE.NAMES = FALSE)

