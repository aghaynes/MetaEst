# FUNCTIONS ----
# SD
se_n_2sd <- function(sd, se, n){
  if(is.null(sd)) sd <- rep(NA, length(se))
  i <- is.na(sd) & !(is.na(se) & is.na(n))
  sd[i] <- (se*sqrt(n))[i]
  sd
}

sdpooled <- function(sdpooled = NULL, n1, n2, sd1, sd2){
  if(is.null(sdpooled)) sdpooled <- rep(NA, length(sd1))
  i <- is.na(sdpooled) & !(is.na(n1) & is.na(n2) & is.na(sd1) & is.na(sd2))
  sdpooled[i] <- sqrt(((n1-1)*(sd1^2) + (n2-1)*(sd2^2))/(n1+n2-2))[i]
  sdpooled
}
sdchange <- function(sdchange = NULL, sd1 = NULL, sd2 = NULL, sdpooled = NULL, r){
  if(is.null(sdchange)) sdchange <- rep(NA, length(r))
  if(is.null(sd1)) sd1 <- rep(NA, length(r))
  if(is.null(sd2)) sd2 <- rep(NA, length(r))
  if(is.null(sdpooled)) sdpooled <- rep(NA, length(r))
  
  i <- is.na(sdchange) & !(is.na(sd1) & is.na(sd2)) # indicator for replacement
  
  sdchange[i] <- sqrt(sd1^2 + sd2^2 - 2*r*sd1*sd2)[i]
  
  i <- is.na(sdchange) & !is.na(sdpooled)  # indicator for replacement
  
  sdchange[i] <- sqrt(2*(1-r)*sdpooled^2)[i]
  sdchange
  
  # ADD SD_diff = SE_diff/(1/n_1 +1/n_2)
}


# SE
mean_t_2se <- function(se, mean, t){
  if(is.null(se)) se <- rep(NA, length(mean))
  i <- is.na(se) & !(is.na(mean) & is.na(t))
  se[i] <- (mean/t)[i]
  abs(se)
}
sd_n_2se <- function(se, sd, n){
  if(is.null(se)) se <- rep(NA, length(sd))
  
  i <- is.na(se) & !(is.na(sd) & is.na(n))
  se[i] <- (sd/sqrt(n))[i]
  se
}
uci_lci_p_2se <- function(se, uci, lci, level){
  if(is.null(se)) se <- rep(NA, length(uci))
  
  i <- is.na(se) & !(is.na(uci) & is.na(lci) & is.na(level))
  
  se[i] <- abs((uci - lci)/(2*qnorm((100-level)/(2*100))))[i]
  se
}
ci_mean_p_2se <- function(se = NULL, mean, uci=NULL, lci=NULL, ulevel){
  if(is.null(uci)) uci <- rep(NA, length(mean))
  if(is.null(lci)) lci <- rep(NA, length(mean))
  if(is.null(se)) se <- rep(NA, length(mean))
  
  cil <- mean - lci
  se[is.na(se) & !is.na(cil)] <- abs(cil/qnorm((100-ulevel)/(2*100)))[is.na(se) & !is.na(cil)]
  
  cil <- uci - mean
  se[is.na(se) & !is.na(cil)] <- abs(cil/qnorm((100-ulevel)/(2*100)))[is.na(se) & !is.na(cil)]
  
  se
}

se_est <- function(se = NULL, mean = NULL, sd = NULL, uci = NULL, lci = NULL, level = NULL, ulevel = NULL, rep = FALSE){
  n <- max(length(se), length(mean), length(sd), length(uci), length(lci), length(level), length(ulevel))
  for(i in c("se", "mean", "sd", "uci", "lci", "level", "ulevel")){
    if(eval(parse(text = paste("is.null(", i, ")")))) {
      eval(parse(text = paste(i, " <- rep(NA, ", n, ")")))
    }
  }
  
  n_na1 <- length(is.na(se))
  
  while(n_na2 < n_na1){
    n_na1 <- length(is.na(se))
    
    se <- sd_n_2se(se, sd, n)
    se <- uci_lci_p_2se(se, uci, lci, level)
    se <- ci_mean_p_2se(se, mean, uci, lci, ulevel)
    se <- mean_t_2se(se, mean, t)
    
    n_na2 <- length(is.na(se))
  }
  print(paste(n_na2, "of", n_na1, "NAs replaced"))
  
  se
}

diff_se <- function(diff_se = NULL, sd, n1, n2){
  if(is.null(diff_se)) diff_se <- rep(NA, length(sd))
  diff_se[is.na(diff_se)] <- sd * sqrt((1/n1)+(1/n2))[is.na(diff_se)]
  diff_se
}

diff_sd <- function(diff_sd = NULL, sd1, sd2, n1, n2){
  if(is.null(diff_sd)) diff_sd <- rep(NA, length(sd1))
  i <- is.na(diff_sd)
  diff_sd[i] <- (sqrt(((n1-1)*(sd1^2)+(n2-1)*(sd2^2))/(n1+n2-2)))[i]
  diff_sd
}
 

# mean
se_t_2mean <- function(mean = NULL, se, t){
  if(is.null(mean)) mean <- rep(NA, length(se))
  i <- is.na(mean) & !(is.na(se) & is.na(t))
  mean[i] <- (-se*t)[i]
  mean
}




