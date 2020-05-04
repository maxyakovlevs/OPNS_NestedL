person_id = 10000


df_twostage<-readRDS('/Users/yakov/Downloads/structural estimation/OPNS_NestedL/variables/values.rds')
df_singleshot<-readRDS('/Users/yakov/Downloads/structural estimation/OPNS_NestedL/variables/values.rds')


###Two-stage Nested Logit specification
inclusive <- sqldf("select distinct(bucket) from df_twostage")
inclusive$inclusive_value[1] <- df_twostage$lambda[1] * log(sum(exp(df_twostage$val[1]/df_twostage$lambda[1]) + exp(df_twostage$val[4]/df_twostage$lambda[1]) + exp(df_twostage$val[7]/df_twostage$lambda[1])))
inclusive$inclusive_value[2] <- df_twostage$lambda[2] * log(sum(exp(df_twostage$val[2]/df_twostage$lambda[2]) + exp(df_twostage$val[5]/df_twostage$lambda[2]) + exp(df_twostage$val[8]/df_twostage$lambda[2])))
inclusive$inclusive_value[3] <- df_twostage$lambda[3] * log(sum(exp(df_twostage$val[3]/df_twostage$lambda[3]) + exp(df_twostage$val[6]/df_twostage$lambda[3]) + exp(df_twostage$val[9]/df_twostage$lambda[3])))


df_twostage_combo <- sqldf("select * from df_twostage left join inclusive on df_twostage.bucket = inclusive.bucket")
names(df_twostage_combo)[5] <- "drop"
df_twostage_combo<-select(df_twostage_combo, -drop)
df_twostage_bucket <-
  df_twostage_combo %>%
  distinct(bucket, inclusive_value)

twostage_first <- merge(df_twostage_bucket, data.frame(row_id = seq(person_id))) %>%
  group_by(bucket) %>%
  arrange(bucket) %>%
  mutate(inter_util = inclusive_value + rgev(n(), xi = 0, mu = 0, beta = 1)) 

  twostage_final <- sqldf("select * from twostage_first left join df_twostage on twostage_first.bucket = df_twostage.bucket") 
  names(twostage_final)[1] <- "dropbucket"
  twostage_final<-select(twostage_final, -dropbucket) %>%
  mutate(final_util = val + lambda * rgev(n(), xi = 0, mu = 0, beta = 1)) %>%
  group_by(row_id) %>%
  dplyr::filter(final_util == max(final_util)) %>%
  select(row_id, bucket, choice,final_util)

###single-shot specification - simultaneous shock resolution  
  inclusive2 <- sqldf("select distinct(bucket) from df_singleshot")
  inclusive2$inclusive_value[1] <- df_singleshot$lambda[1] * log(sum(exp(df_singleshot$val[1]/df_singleshot$lambda[1]) + exp(df_singleshot$val[4]/df_singleshot$lambda[1]) + exp(df_singleshot$val[7]/df_singleshot$lambda[1])))
  inclusive2$inclusive_value[2] <- df_singleshot$lambda[2] * log(sum(exp(df_singleshot$val[2]/df_singleshot$lambda[2]) + exp(df_singleshot$val[5]/df_singleshot$lambda[2]) + exp(df_singleshot$val[8]/df_singleshot$lambda[2])))
  inclusive2$inclusive_value[3] <- df_singleshot$lambda[3] * log(sum(exp(df_singleshot$val[3]/df_singleshot$lambda[3]) + exp(df_singleshot$val[6]/df_singleshot$lambda[3]) + exp(df_singleshot$val[9]/df_singleshot$lambda[3])))
  
  df_singleshot_combo <- sqldf("select * from df_singleshot left join inclusive2 on df_singleshot.bucket = inclusive2.bucket")
  names(df_singleshot_combo)[5] <- "drop"
  df_singleshot_combo<-select(df_singleshot_combo, -drop)
  
 singleshot_first <-
    df_singleshot_combo %>%
    distinct(bucket, inclusive_value)
  
 singleshot_second <- merge(singleshot_first, data.frame(row_id = seq(person_id))) %>%
    group_by(bucket) %>%
    arrange(bucket)  %>%
  mutate(inter_util = inclusive_value) 
  
  singleshot_final <- sqldf("select * from singleshot_second left join df_singleshot on singleshot_second.bucket = df_singleshot.bucket") 
  names(singleshot_final)[1] <- "dropbucket"
  singleshot_final<-select(singleshot_final, -dropbucket) %>%
    mutate(final_util = val + lambda * rgev(n(), xi = 0, mu = 0, beta = 1)+rgev(n(), xi = 0, mu = 0, beta = 1)) %>%
    group_by(row_id) %>%
    dplyr::filter(final_util == max(final_util)) %>%
    select(row_id, bucket, choice,final_util)
  

###Obtaining empirical choice probs from data
  
#Getting specification vals for counting
twostage_final$specification<-ifelse((twostage_final$bucket == 1 & twostage_final$choice == "A"), 1,ifelse((twostage_final$bucket == 1 & twostage_final$choice == "B"), 2,
                                                        ifelse((twostage_final$bucket == 1 & twostage_final$choice == "C"),3,
                                                        ifelse((twostage_final$bucket == 2 & twostage_final$choice == "A"), 4,
                                                         ifelse((twostage_final$bucket == 2 & twostage_final$choice == "B"), 5,
                                                         ifelse((twostage_final$bucket == 2 & twostage_final$choice == "C"),6,
                                                        ifelse((twostage_final$bucket == 3 & twostage_final$choice == "A"), 7,
                                                               ifelse((twostage_final$bucket == 3 & twostage_final$choice == "B"), 8,
                                                                      ifelse((twostage_final$bucket == 3 & twostage_final$choice == "C"),9,0)))))))))

singleshot_final$specification<-ifelse((singleshot_final$bucket == 1 & singleshot_final$choice == "A"), 1,ifelse((singleshot_final$bucket == 1 & singleshot_final$choice == "B"), 2,
                                                                                     ifelse((singleshot_final$bucket == 1 & singleshot_final$choice == "C"),3,
                                                                                     ifelse((singleshot_final$bucket == 2 & singleshot_final$choice == "A"), 4,
                                                                                     ifelse((singleshot_final$bucket == 2 & singleshot_final$choice == "B"), 5,
                                                                                    ifelse((singleshot_final$bucket == 2 & singleshot_final$choice == "C"),6,
                                                                                       ifelse((singleshot_final$bucket == 3 & singleshot_final$choice == "A"), 7,
                                                                                       ifelse((singleshot_final$bucket == 3 & singleshot_final$choice == "B"), 8,
                                                                                        ifelse((singleshot_final$bucket == 3 & singleshot_final$choice == "C"),9,0)))))))))
#Calculating empirical frequencies and probs

count<-table(twostage_final$specification)
phat<-count/10000

#Obtaining bootstrap covariance matrix
prob<-function(d,i){
  d2<-d[i,]
  return (table(d2$specification)/10000)
}

boot.results<-boot(twostage_final,prob,R=1000)
boot.results
boot.results$t0


cov.mat<-cov(boot.results$t)



boot.results2<-boot(singleshot_final,prob,R=1000)
boot.results2
boot.results2$t0


cov.mat2<-cov(boot.results2$t)


#theoretical probs

t_prob <- function(df){
  prob <- df %>%
    mutate(v = exp(val/lambda)) %>%
    group_by(bucket) %>%
    mutate(inner_sum = sum(v)) %>%
    mutate(interim_prob = v * (inner_sum ^ (lambda - 1))) %>%
    ungroup() %>%
    mutate(final_prob = interim_prob / sum(unique(inner_sum)^unique(lambda))) %>%
    select(bucket, choice, final_prob)
  
  return(prob)
}
probs_results<-t_prob(values)
values$t_probs<-probs_results$final_prob

#Wald test

prob_diff1<-(values$t_probs - boot.results$t0)
prob_diff2<-(values$t_probs - boot.results2$t0)
q1<--t(prob_diff1)%*%cov.mat%*%prob_diff1
q2<--t(prob_diff1)%*%cov.mat2%*%prob_diff1
q1
q2

wald.test(b = prob_diff1,sigma = cov.mat, Terms = 1:8)
wald.test(b = prob_diff2,sigma = cov.mat, Terms = 1:8)
