library(catR)
library(tidyverse)
library(car)

# read in item parameters for full ACOM
acom_itpar_content <- read.csv(file = "acom_Tscaled_ItemParameters_Content.csv",na.strings = "NA")
acom_itpar_content$itnum <- as.numeric(seq(1:59)) 

acom_itpar <- acom_itpar_content[1:59,c(4:7,3)]
acom_itpar <- data.matrix(acom_itpar)

# Use recode function in car package to collapse categories
# assumes data are coded such that:
# cannot do because of my communication problem = 0
# cannot do for some other reason = NA
# not very = 0
# somewhat = 1
# mostly = 2
# completely = 3
#
# acom_itpar$merge_cats contains codes 0:4 that define how to collapse categories
# no recode necessary for acom_itpar$merge_cats = 0
# recode(A021,"1=0;2=1;3=2") # acom_itpar$merge_cats = 1; mplus: 1 2
# recode(A028,"2=1;3=2") # acom_itpar$merge_cats = 2; mplus: 0 2
# recode(A011,"3=2") # acom_itpar$merge_cats = 3; mplus: 0 1
# recode(A082,"2:3=1") # acom_itpar$merge_cats = 4; mplus: 0; only item 82

# create response string with constant response of "mostly"
resp_vec_59 <- rep(2,59)
resp_vec_59r <- resp_vec_59
# recode resp_vec
resp_vec_59r[which(acom_itpar_content$merge_cats==1)] <- 
  recode(resp_vec_59[which(acom_itpar_content$merge_cats==1)], "1=0;2=1;3=2")
resp_vec_59r[which(acom_itpar_content$merge_cats==2)] <- 
  recode(resp_vec_59[which(acom_itpar_content$merge_cats==2)], "2=1;3=2")
resp_vec_59r[which(acom_itpar_content$merge_cats==3)] <- 
  recode(resp_vec_59[which(acom_itpar_content$merge_cats==3)], "3=2")
resp_vec_59r[which(acom_itpar_content$merge_cats==4)] <- 
  recode(resp_vec_59[which(acom_itpar_content$merge_cats==4)], "2:3=1")

resp_vec_59r <- as.numeric(resp_vec_59r)

theta_mostly <- eapEst(it = acom_itpar[,1:4], x = resp_vec_59r, 
                    model = "GRM", D = 1, priorDist = "norm", priorPar = c(50, 10), 
                    lower = 10, upper = 90, nqp = 33)


sem_mostly<- eapSem(theta_mostly, it = acom_itpar[,1:4], x = resp_vec_59r, model = "GRM", D = 1,
                    priorDist = "norm", priorPar = c(50, 10), lower = 10, upper = 90, nqp = 33)



# this vector codes the item order you get from entering a constant response of "mostly" in the Java app
mostly_itorder <- c("A004", "A120", "A012", "A055", "A080", "A141", 
"A022", "A123", "A019", "A161", "A029", "A079", "A016", "A060", "A023", "A162", 
"A124", "A170", "A096", "A177", "A095", "A082", "A089", "A074", "A116", "A064", 
"A011", "A066", "A125", "A098", "A092", "A152", "A135", "A173", "A101", "A158", 
"A138", "A031", "A018", "A167", "A072", "A038", "A104", "A164", "A032", "A069",
"A028", "A166", "A160", "A063", "A117", "A114", "A103", "A147", "A100", "A099", "A065", "A021", "A090")


# confirm same score estimate and sem with re-ordered data
ord_vec <- 1
for (i in 2:59) {
  ord_vec[i] <- which(acom_itpar_content$item==mostly_itorder[i])
}

theta_mostly_javacatord <- eapEst(it = acom_itpar[ord_vec[1:59],1:4], 
       x = c(resp_vec_59r[ord_vec[1:59]]), 
       model = "GRM", D = 1, priorDist = "norm", priorPar = c(50, 10), 
       lower = 10, upper = 90, nqp = 33)


sem_mostly_javacatord <- eapSem(theta_mostly_javacatord,it = acom_itpar[ord_vec[1:59],1:4], 
                            x = c(resp_vec_59r[ord_vec[1:59]]), model = "GRM", D = 1, 
                            priorDist = "norm", priorPar = c(50, 10), lower = 10, upper = 90, nqp = 33)




# breakBank(acom_itpar)

# define options for content balancing
acom_cb_names <- c("talk","comp","gen","writ","nam")
acom_cb_props <- c(0.355932203, 0.271186441, 0.13559322, 0.13559322, 0.101694915)
acom_cbControl <- list(names = acom_cb_names, props = acom_cb_props)
acom_cb_group <- acom_itpar_content$content_area
test.cbList(acom_cbControl, cbGroup = acom_cb_group)

test_record <- matrix(data = NA, nrow = 59, ncol = 6, byrow = TRUE, 
                      dimnames = list(NULL,c("Item Number", "Item Content", 
                                             "Response", "Response Value", 
                                             "Score Estimate", "Standard Error")))
test_record <- as_tibble(test_record)

# specify initial assumed score as population average
theta_init <- 50
resp_vec <- resp_vec_59r
# select A004 as first item; it provides the most information at theta = 50
it1 <- startItems(acom_itpar, model = "GRM", fixItems = 1)


# specify A004 as administered
it_out <- it1$items

#estimate theta and sem
cur_theta <- eapEst(it = acom_itpar[1,1:4], x = resp_vec[1], model = "GRM", D = 1, priorDist = "norm", priorPar = c(50, 10), 
       lower = 10, upper = 90, nqp = 33)
cur_sem <- eapSem(cur_theta, it = acom_itpar[1,1:4], x = resp_vec[1], model = "GRM", D = 1, priorDist = "norm", 
                  priorPar = c(50, 10), lower = 10, upper = 90, nqp = 33)


test_record$`Item Number`[1] <- 1
test_record$`Item Content`[1] <- acom_itpar_content$item_content[1]
test_record$Response[1] <- resp_vec[1]
test_record$`Score Estimate`[1] <- cur_theta
test_record$`Standard Error`[1] <- cur_sem

for (i in 2:59) {
it_next <- nextItem(itemBank = acom_itpar[,1:4], model = "GRM", theta = cur_theta, out = test_record$`Item Number`[1:i-1], 
                    x = NULL, criterion = "MFI", method = "EAP", priorDist = "norm", priorPar = c(50, 10),
                    D = 1, range = c(10, 90), parInt = c(10, 90, 33), infoType = "observed", 
                    randomesque = 1, random.seed = NULL, rule = "length", thr = 20, SETH = NULL, 
                    AP = 1, nAvailable = NULL, maxItems = 59, cbControl = acom_cbControl, cbGroup = acom_cb_group)



test_record$`Item Number`[i] <- it_next$item
test_record$`Item Content`[i] <- acom_itpar_content$item_content[it_next$item]
test_record$Response[i] <- resp_vec[it_next$item]

cur_theta <- eapEst(it = acom_itpar[test_record$`Item Number`[1:i],1:4], x = test_record$Response[1:i], 
                    model = "GRM", D = 1, priorDist = "norm", priorPar = c(50, 10), 
                    lower = 10, upper = 90, nqp = 33)
cur_sem <- eapSem(cur_theta, it = acom_itpar[test_record$`Item Number`[1:i],1:4], 
                  x = test_record$Response[1:i], model = "GRM", D = 1, priorDist = "norm", 
                  priorPar = c(50, 10), lower = 10, upper = 90, nqp = 33)

test_record$`Score Estimate`[i] <- cur_theta
test_record$`Standard Error`[i] <- cur_sem
}
