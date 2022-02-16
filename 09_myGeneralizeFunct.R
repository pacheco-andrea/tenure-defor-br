# ************************************************************ #
# Tweak generalize functions to use in analysis
# write in my specifications for lasso 
# ************************************************************ #


myGeneralize <- function(outcome, treatment, trial, selection_covariates, data, method = "weighting",
                         selection_method = "lr", sl_library = NULL, survey_weights = FALSE, trim_weights=FALSE, 
                         trim_pctile = .97, is_data_disjoint = TRUE, trimpop = FALSE, seed, formula=NULL){
  
  ##### make methods lower case #####
  method = tolower(method)
  selection_method = tolower(selection_method)
  
  ### If using TMLE, must trim target population
  if(method == "tmle"){
    trimpop = TRUE
  }
  
  ##### CHECKS #####
  if (!is.data.frame(data)){
    stop("Data must be a data.frame.", call. = FALSE)
  }
  
  if(is.null(outcome) | anyNA(match(outcome,names(data)))){
    stop("Outcome is not a variable in the data provided!",call. = FALSE)
  }
  
  if(is.null(treatment) | anyNA(match(treatment,names(data)))){
    stop("Treatment is not a variable in the data provided!",call. = FALSE)
  }
  
  if(is.null(selection_covariates) | anyNA(match(selection_covariates,names(data)))){
    stop("Not all covariates listed are variables in the data provided!",call. = FALSE)
  }
  
  if(!length(na.omit(unique(data[,trial]))) == 2){
    stop("Trial Membership variable not binary", call. = FALSE)
  }
  
  if(anyNA(match(names(table(data[,trial])),c("0","1")))){
    stop("Sample Membership variable must be coded as `0` (not in trial) or `1` (in trial)",call. = FALSE)
  }
  
  if(!length(na.omit(unique(data[,treatment]))) == 2){
    stop("Treatment variable not binary", call. = FALSE)
  }
  
  if(!method %in% c("weighting","bart","tmle")){
    stop("Invalid method!",call. = FALSE)
  }
  
  if(!selection_method %in% c("lr","rf","lasso","gbm","super")){
    stop("Invalid weighting method!",call. = FALSE)
  }
  
  if(!missing(seed)){
    if(!is.numeric(seed)){
      stop("seed must be numeric!,call. = FALSE")
    }}
  
  ##### trim population #####
  if(trimpop == FALSE){
    n_excluded = NULL
    ## just keep the data we need
    if(survey_weights == FALSE){
      data = data[rownames(na.omit(data[,c(trial,selection_covariates)])),c(outcome, treatment, trial, selection_covariates)]
    } else{
      data = data[rownames(na.omit(data[,c(trial,selection_covariates)])),c(outcome, treatment, trial, survey_weights, selection_covariates)]
    }
  }
  
  if(trimpop == TRUE){
    n_excluded = trim_pop(trial, selection_covariates, data)$n_excluded
    data = trim_pop(trial, selection_covariates, data)$trimmed_data
  }
  
  ##### Weighting object for diagnostics #####
  weight_object = weighting(outcome, treatment, trial, selection_covariates, data, selection_method, sl_library, survey_weights, trim_weights, trim_pctile, is_data_disjoint,seed)
  
  participation_probs = weight_object$participation_probs
  weights = weight_object$weights
  g_index = gen_index(participation_probs$population, participation_probs$trial)
  
  ##### Generalize results #####
  ## First, estimate SATE
  
  if(is.null(formula)){
    
    formula = as.formula(paste(outcome,treatment,sep="~"))
    if(length(table(data[,outcome]))!=2)
    {
      SATE_model = lm(as.formula(paste(outcome,treatment,sep="~")), data = data)
      SATE = summary(SATE_model)$coefficients[treatment, "Estimate"]
      SATE_se = summary(SATE_model)$coefficients[treatment, "Std. Error"]
      
      SATE_CI_l = SATE - 1.96*SATE_se
      SATE_CI_u = SATE + 1.96*SATE_se
    }else{
      SATE_model = glm(as.formula(paste(outcome,treatment,sep="~")), data = data, family = 'quasibinomial')
      SATE = exp(summary(SATE_model)$coefficients[treatment,"Estimate"])
      SATE_se = NA
      
      SATE_CI_l = as.numeric(exp(confint(SATE_model)[treatment,]))[1]
      SATE_CI_u = as.numeric(exp(confint(SATE_model)[treatment,]))[2]
    }
  }
  
  ATE_design = survey::svydesign(id = ~1, data = data[which(data[,trial] == 1),], weights = data$weights[which(data[,trial] == 1)])
  
  if(!is.null(formula)){
    
    if(nrow(data) < 40){
      stop("error: insufficient observations for regression to run", call. = FALSE)
    }
    if(nrow(data) > 40){
      if(length(unique(data$state)) > 1){ # if there is more than one state in this dataset, run the normal model:
        
        SATE_model <- miceadds::glm.cluster(data = data, formula = as.formula(paste0("cbind(for_ch, (for_orig-for_ch)) ~ ",
                                                                                     names(data)[grep("_vs_", names(data))],
                                                                                     " + elevation + slope + travel_time + pop + area + state")),
                                            cluster="mun", family = binomial(link = "logit"))
      }
      
      if(length(unique(data$state)) <= 1){ 
        SATE_model <- miceadds::glm.cluster(data = data, formula = as.formula(paste0("cbind(for_ch, (for_orig-for_ch)) ~ ",
                                                                                     names(data)[grep("_vs_", names(data))],
                                                                                     " + elevation + slope + travel_time + pop + area")),
                                            cluster="mun", family = binomial(link = "logit"))
      }
    }
    # calculate marginal effects
    ame <- summary(margins(model=SATE_model$glm_res, vcov= SATE_model$vcov, data=data, type = "response"))
    
    # establish results
    SATE <- ame[grep(treatment, ame$factor),]$AME
    SATE_se <- ame[grep(treatment, ame$factor),]$SE
    SATE_CI_l <- ame[grep(treatment, ame$factor),]$lower
    SATE_CI_u <- ame[grep(treatment, ame$factor),]$upper
  }
  
  SATE_results = list(estimate = SATE,
                      se = SATE_se,
                      CI_l = SATE_CI_l,
                      CI_u = SATE_CI_u)
  
  ## Weighting results
  if(method == "weighting"){
    TATE_results = weight_object$TATE
  }
  
  ## BART results
  if(method == "bart"){
    TATE_results = generalize_bart(outcome, treatment, trial, selection_covariates,data,seed)$TATE
  }
  
  ## TMLE results
  if(method == "tmle"){
    TATE_results = generalize_tmle(outcome, treatment, trial, selection_covariates, data,seed)$TATE
  }
  
  ##### sample size of trial and population #####
  n_trial = nrow(data[which(data[,trial] == 1),])
  n_pop = nrow(data[which(data[,trial] == 0),])
  
  ##### if using weighting method, insert a weighted covariates table
  weighted_cov_tab = NULL
  if(method == "weighting"){
    weighted_cov_tab = covariate_table(trial = trial, selection_covariates = selection_covariates, data = data,
                                       weighted_table = TRUE, selection_method = selection_method, sl_library = sl_library, survey_weights=survey_weights,
                                       trim_weights=trim_weights, trim_pctile=trim_pctile,is_data_disjoint = is_data_disjoint)
  }
  
  if(survey_weights == FALSE){
    data_output = data[,c(outcome, treatment, trial, selection_covariates)]
    n_pop_eff = NA
  } else{
    data_output = data[,c(outcome, treatment, trial, selection_covariates,survey_weights)]
    n_pop_eff = ceiling(sum(data[which(data[,trial]==0),survey_weights], na.rm = TRUE))
  }
  
  ##### Items to save to "generalize" object #####
  out = list(SATE = SATE_results,
             TATE = TATE_results,
             outcome = outcome,
             treatment = treatment,
             trial = trial,
             method = method,
             selection_method = selection_method,
             g_index = g_index,
             n_trial = n_trial,
             n_pop = n_pop,
             n_pop_eff = n_pop_eff,
             trimpop = trimpop,
             n_excluded = n_excluded,
             selection_covariates = selection_covariates,
             weighted_covariate_table = weighted_cov_tab,
             data = data_output,
             survey_weights = (survey_weights != FALSE),
             is_data_disjoint = is_data_disjoint)
  
  class(out) = "generalize"
  
  return(out)
}

# trial = "trial"
# selection_covariates = covs
# data = myData
# selection_method = "lasso"
# trimpop = F
# survey_weights = FALSE
# trimpop = TRUE
# is_data_disjoint = TRUE
# seed=13783


myAssess <- function (trial, selection_covariates, data, selection_method = "lr", 
                      sl_library = NULL, survey_weights = FALSE, trim_weights = FALSE, 
                      trim_pctile = 0.97, is_data_disjoint = TRUE, trimpop = FALSE, 
                      seed) {
  selection_method = tolower(selection_method)
  if (!is.data.frame(data)) {
    stop("Data must be a data.frame.", call. = FALSE)
  }
  if (anyNA(match(selection_covariates, names(data)))) {
    stop("Not all covariates listed are variables in the data provided!", 
         call. = FALSE)
  }
  if (!length(na.omit(unique(data[, trial]))) == 2) {
    stop("Trial Membership variable not binary", call. = FALSE)
  }
  if (anyNA(match(names(table(data[, trial])), c("0", "1")))) {
    stop("Sample Membership variable must be coded as `0` (not in trial) or `1` (in trial)", 
         call. = FALSE)
  }
  if (!selection_method %in% c("lr", "rf", "lasso", "gbm", 
                               "super")) {
    stop("Invalid weighting method!", call. = FALSE)
  }
  if (!missing(seed)) {
    if (!is.numeric(seed)) {
      stop("seed must be numeric!,call. = FALSE")
    }
  }
  if (trimpop == FALSE) {
    n_excluded = NULL
  }
  if (trimpop == TRUE) {
    n_excluded = trim_pop(trial, selection_covariates, data)$n_excluded
    data = trim_pop(trial, selection_covariates, data)$trimmed_data
  }
  # original code for getting weights and g_index
  # weight_object = weighting(outcome = NULL, treatment = NULL, 
  #                           trial, selection_covariates, data, selection_method, 
  #                           sl_library, survey_weights, trim_weights, trim_pctile, 
  #                           is_data_disjoint, seed)
  # participation_probs = weight_object$participation_probs
  # weights = weight_object$weights
  # 
  # g_index = gen_index(participation_probs$trial, participation_probs$population)
  
  # my weighing bare basics:
  test.x = model.matrix(~-1 + ., data = data[, selection_covariates])
  test.y = data[, trial]
  # predict participation probabilites: s = "lambda.1se", 
  # note - this is where the bug was. i tweaked the number of folds
  fit <- glmnet::cv.glmnet(x = test.x, y = test.y, family="binomial", nfolds=5)
  ps = as.numeric(predict(fit, newx = test.x, s = fit$lambda.min, alignment = "fraction", type = "response"))
  # "trial" participation probabilities
  participation_probs <- list(population = ps[which(data[, trial] == 0)], trial = ps[which(data[, trial] == 1)])
  lapply(participation_probs, head)
  # generate g_index
  g_index <- gen_index(round(participation_probs$trial, digits=4), round(participation_probs$population, digits=4))
  g_index
  # weights are the inverse of the trial participation probability
  data$weights = ifelse(data[, trial] == 0, 0, (1 - ps)/ps)
  
  # create my weight object:
  weight_object = list(participation_probs = participation_probs, weights = data$weights, 
                       TATE = NULL)
  
  cov_tab = covariate_table(trial = trial, selection_covariates = selection_covariates, 
                            data = data, weighted_table = FALSE, survey_weights = survey_weights)
  # weighted_cov_tab = covariate_table(trial = trial, selection_covariates = selection_covariates,
  #                                    data = data, weighted_table = TRUE, selection_method = selection_method,
  #                                    sl_library = sl_library, survey_weights = survey_weights,
  #                                    trim_weights = trim_weights, trim_pctile = trim_pctile,
  #                                    is_data_disjoint = is_data_disjoint)
  n_trial = nrow(data[which(data[, trial] == 1), ])
  n_pop = nrow(data[which(data[, trial] == 0), ])
  if (survey_weights == FALSE) {
    data_output = data[, c(trial, selection_covariates)]
    n_pop_eff = NA
  }
  # else {
  #   data_output = data[, c(trial, selection_covariates, survey_weights)]
  #   n_pop_eff = ceiling(sum(data[which(data[, trial] == 0), 
  #                                survey_weights], na.rm = TRUE))
  # }
  out = list(g_index = g_index, selection_method = selection_method, 
             selection_covariates = selection_covariates, trial_name = trial, 
             n_trial = n_trial, n_pop = n_pop, n_pop_eff = n_pop_eff, 
             trimpop = trimpop, n_excluded = n_excluded, participation_probs = participation_probs, 
             weights = weight_object$weights, survey_weights = (survey_weights != FALSE), covariate_table = cov_tab, 
             data = data_output)
  class(out) = "generalize_assess"
  return(out)
}



