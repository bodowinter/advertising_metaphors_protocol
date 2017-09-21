## Bodo Winter
## September 20, 2017
## Calculate inter-rater reliability for coding

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Load in libraries:

library(tidyverse)
library(xlsx)
library(irr)

## Load in data:

setwd('/Users/winterb/Research/sam_advertising/IRR/data/')
IRR1 <- read.xlsx('IRR_round_1.xlsx',
	sheetIndex = 1, stringsAsFactors = F) %>% as_tibble()
IRR2 <- read.xlsx('IRR_round_2.xlsx',
	sheetIndex = 1, stringsAsFactors = F) %>% as_tibble()
IRR3 <- read.xlsx('IRR_round_3.xlsx',
	sheetIndex = 1, stringsAsFactors = F) %>% as_tibble()

## Rename the 'Paula' column:

IRR2 <- rename(IRR2,
	Paula_figop = Paula)

## Fix the one NA for IRR3, which should be the same as Sam's:

IRR3[is.na(IRR3$Paula_figop), ]$Paula_figop <- IRR3[is.na(IRR3$Paula_figop), ]$Sam_figop



##------------------------------------------------------------------
## Calculate IRRs, round #1:
##------------------------------------------------------------------

## Perform IRR for round #1:

kappam.fleiss(select(IRR1, Paula_figop, Sam_figop))


##------------------------------------------------------------------
## Calculate IRRs, round #2:
##------------------------------------------------------------------

## Perform changes for consistent labeling:

IRR2[IRR2$Sam_figop == 'Phone, you', ]$Sam_figop <- 'Phone'
IRR2[IRR2$Paula_figop == 'Phone, you', ]$Paula_figop <- 'Phone'
IRR2[IRR2$Sam_figop == 'Phone, text', ]$Sam_figop <- 'Phone'
IRR2[IRR2$Paula_figop == 'Phone, text', ]$Paula_figop <- 'Phone'
IRR2[IRR2$Sam_figop == 'Siri and iPhone', ]$Sam_figop <- 'iPhone'
IRR2[IRR2$Paula_figop == 'Siri and iPhone', ]$Paula_figop <- 'iPhone'
IRR2[IRR2$Sam_figop == 'OM4G ', ]$Sam_figop <- 'OM4G'
IRR2[IRR2$Paula_figop == 'OM4G ', ]$Paula_figop <- 'OM4G'
IRR2[IRR2$Sam_figop == '4G ', ]$Sam_figop <- '4G'
IRR2[IRR2$Paula_figop == '4G ', ]$Paula_figop <- '4G'
IRR2[IRR2$Sam_figop == 'Tesco Mobile as a serious contender in competing mobile network companies', ]$Sam_figop <- 'Tesco Mobile '
IRR2[IRR2$Paula_figop == 'Tesco gets you a Samsung Galaxy and a good service on any network', ]$Paula_figop <- 'Tesco Mobile '

## Get subsets:

IRR2_step1 <- filter(IRR2, Step == 1) %>%
	select(Sam_figop, Paula_figop)
IRR2_step2 <- filter(IRR2, Step == 2) %>%
	select(Sam_figop, Paula_figop)
IRR2_step3 <- filter(IRR2, Step == 3) %>%
	select(Sam_figop, Paula_figop)
IRR2_step4 <- filter(IRR2, Step == 4) %>%
	select(Sam_figop, Paula_figop)
IRR2_step5 <- filter(IRR2, Step == 5) %>%
	select(Sam_figop, Paula_figop)

## Perform IRRs:

kappam.fleiss(IRR2_step1, detail = TRUE)
kappam.fleiss(IRR2_step2, detail = TRUE)
kappam.fleiss(IRR2_step3, detail = TRUE)
kappam.fleiss(IRR2_step4, detail = TRUE)
kappam.fleiss(IRR2_step5, detail = TRUE)



##------------------------------------------------------------------
## Calculate IRRs, round #3:
##------------------------------------------------------------------

## Using which() here due to missingness:

IRR3[which(IRR3$Sam_figop == 'Giffgaff network are expressing their respect of @LayolaLotus in choosing their network over competitors'), ]$Sam_figop <- 'Giffgaff'
IRR3[which(IRR3$Paula_figop == 'Giffgaff network are expressing their respect of @LayolaLotus in choosing their network over competitors'), ]$Paula_figop <- 'Giffgaff'
IRR3[which(IRR3$Paula_figop == 'flick + absent finger (blur) '), ]$Paula_figop <- 'flick + blur '
IRR3[which(IRR3$Sam_figop == 'metaphtonymy '), ]$Sam_figop <- 'Metaphtonymy'
IRR3[which(IRR3$Sam_figop == 'O2 gives you a surprise gift when you top up'), ]$Sam_figop <- 'Surprise gift, O2'
IRR3[which(IRR3$Paula_figop == 'O2 gives you a surprise gift when you top up'), ]$Paula_figop <- 'Surprise gift, O2'

IRR3[which(IRR3$Sam_figop == 'Metaphor and Metonymy'), ]$Sam_figop <- 'Metaphor and metonymy'

## Get subsets:

IRR3_step1 <- filter(IRR3, Step == 1) %>%
	select(Sam_figop, Paula_figop)
IRR3_step2 <- filter(IRR3, Step == 2) %>%
	select(Sam_figop, Paula_figop)
IRR3_step3 <- filter(IRR3, Step == 3) %>%
	select(Sam_figop, Paula_figop)
IRR3_step4 <- filter(IRR3, Step == 4) %>%
	select(Sam_figop, Paula_figop)
IRR3_step5 <- filter(IRR3, Step == 5) %>%
	select(Sam_figop, Paula_figop)

## Perform IRRs:

kappam.fleiss(IRR3_step1, detail = TRUE)
kappam.fleiss(IRR3_step2, detail = TRUE)
kappam.fleiss(IRR3_step3, detail = TRUE)
kappam.fleiss(IRR3_step4, detail = TRUE)
kappam.fleiss(IRR3_step5, detail = TRUE)



##------------------------------------------------------------------
## Create labels for metaphor and metonymy separately, round 2:
##------------------------------------------------------------------

## Which ones count as metaphor:

metaph_yes <- c('Metaphor and metonymy', 'Metaphor', 'Metaphtonymy')
meton_yes <- c('Metaphor and metonymy', 'Metonymy', 'Metaphtonymy', 'Metonymic chain')

## Split up metaphor and metonymy for IRR round 2, step 4:

IRR2_step4$MetaphorSam <- 'None'
IRR2_step4$MetaphorPaula <- 'None'
IRR2_step4$MetonymySam <- 'None'
IRR2_step4$MetonymyPaula <- 'None'

IRR2_step5$MetaphorSam <- 'None'
IRR2_step5$MetaphorPaula <- 'None'
IRR2_step5$MetonymySam <- 'None'
IRR2_step5$MetonymyPaula <- 'None'

IRR2_step4[IRR2_step4$Sam_figop %in% metaph_yes, ]$MetaphorSam <- 'Metaphor'
IRR2_step4[IRR2_step4$Paula_figop %in% metaph_yes, ]$MetaphorPaula <- 'Metaphor'

IRR2_step5[IRR2_step5$Sam_figop %in% metaph_yes, ]$MetaphorSam <- 'Metaphor'
IRR2_step5[IRR2_step5$Paula_figop %in% metaph_yes, ]$MetaphorPaula <- 'Metaphor'

IRR2_step4[IRR2_step4$Sam_figop %in% meton_yes, ]$MetonymySam <- 'Metonymy'
IRR2_step4[IRR2_step4$Paula_figop %in% meton_yes, ]$MetonymyPaula <- 'Metonymy'

IRR2_step5[IRR2_step5$Sam_figop %in% meton_yes, ]$MetonymySam <- 'Metonymy'
IRR2_step5[IRR2_step5$Paula_figop %in% meton_yes, ]$MetonymyPaula <- 'Metonymy'



##------------------------------------------------------------------
## Create labels for metaphor and metonymy separately, round 3:
##------------------------------------------------------------------

## Which ones count as metaphor:

metaph_yes <- c('Metaphor and metonymy', 'Metaphor', 'Metaphtonymy')
meton_yes <- c('Metaphor and metonymy', 'Metonymy', 'Metaphtonymy', 'Metonymic chain')

## Split up metaphor and metonymy for IRR round 2, step 4:

IRR3_step4$MetaphorSam <- 'None'
IRR3_step4$MetaphorPaula <- 'None'
IRR3_step4$MetonymySam <- 'None'
IRR3_step4$MetonymyPaula <- 'None'

IRR3_step5$MetaphorSam <- 'None'
IRR3_step5$MetaphorPaula <- 'None'
IRR3_step5$MetonymySam <- 'None'
IRR3_step5$MetonymyPaula <- 'None'

IRR3_step4[IRR3_step4$Sam_figop %in% metaph_yes, ]$MetaphorSam <- 'Metaphor'
IRR3_step4[IRR3_step4$Paula_figop %in% metaph_yes, ]$MetaphorPaula <- 'Metaphor'

IRR3_step5[IRR3_step5$Sam_figop %in% metaph_yes, ]$MetaphorSam <- 'Metaphor'
IRR3_step5[IRR3_step5$Paula_figop %in% metaph_yes, ]$MetaphorPaula <- 'Metaphor'

IRR3_step4[IRR3_step4$Sam_figop %in% meton_yes, ]$MetonymySam <- 'Metonymy'
IRR3_step4[IRR3_step4$Paula_figop %in% meton_yes, ]$MetonymyPaula <- 'Metonymy'

IRR3_step5[IRR3_step5$Sam_figop %in% meton_yes, ]$MetonymySam <- 'Metonymy'
IRR3_step5[IRR3_step5$Paula_figop %in% meton_yes, ]$MetonymyPaula <- 'Metonymy'



##------------------------------------------------------------------
## Calculate IRRs for round 2 and round 3, metaphor vs. metonymy:
##------------------------------------------------------------------

## Calculate metaphor presence, step 4:

kappam.fleiss(select(IRR2_step4, MetaphorSam:MetaphorPaula))
kappam.fleiss(select(IRR3_step4, MetaphorSam:MetaphorPaula))

## Calculate metaphor presence, step 5:

kappam.fleiss(select(IRR2_step5, MetaphorSam:MetaphorPaula))
kappam.fleiss(select(IRR3_step5, MetaphorSam:MetaphorPaula))

## Calculate metonymy presence, step 4:

kappam.fleiss(select(IRR2_step4, MetonymySam:MetonymyPaula))
kappam.fleiss(select(IRR3_step4, MetonymySam:MetonymyPaula))

## Calculate metonymy presence, step 5:

kappam.fleiss(select(IRR2_step5, MetonymySam:MetonymyPaula))
kappam.fleiss(select(IRR3_step5, MetonymySam:MetonymyPaula))



##------------------------------------------------------------------
## Calculate IRRs for round 2 and round 3 combined:
##------------------------------------------------------------------

## Combine both rounds, step 4:

IRR_both_step4 <- bind_rows(IRR2_step4, IRR3_step4)
# write_csv(IRR_both_step4, 'IRR_both_step4.csv')

## Combine both rounds, step 5:

IRR_both_step5 <- bind_rows(IRR2_step5, IRR3_step5)
# write_csv(IRR_both_step5, 'IRR_both_step5.csv')

## Calculate IRR for step 4 and 5, full data:

kappam.fleiss(select(IRR_both_step4, Sam_figop, Paula_figop))
kappam.fleiss(select(IRR_both_step5, Sam_figop, Paula_figop))

## Calculate IRR for metaphor presence, full data:

kappam.fleiss(select(IRR_both_step4, MetaphorSam:MetaphorPaula))
kappam.fleiss(select(IRR_both_step5, MetaphorSam:MetaphorPaula))

## Calculate IRR for metaphor presence, full data:

kappam.fleiss(select(IRR_both_step4, MetonymySam:MetonymyPaula))
kappam.fleiss(select(IRR_both_step5, MetonymySam:MetonymyPaula))



##------------------------------------------------------------------
## Observed agreements:
##------------------------------------------------------------------

## Observed agreement, round 2:

sum(IRR2_step4$Sam_figop == IRR2_step4$Paula_figop) / nrow(IRR2_step4)
sum(IRR2_step5$Sam_figop == IRR2_step5$Paula_figop) / nrow(IRR2_step5)

## Observed agreement, round 3:

sum(IRR3_step4$Sam_figop == IRR3_step4$Paula_figop) / nrow(IRR3_step4)
sum(IRR3_step5$Sam_figop == IRR3_step5$Paula_figop) / nrow(IRR3_step5)

## Observed agreement both:

sum(IRR_both_step4$Sam_figop == IRR_both_step4$Paula_figop) / nrow(IRR_both_step4)
sum(IRR_both_step5$Sam_figop == IRR_both_step5$Paula_figop) / nrow(IRR_both_step5)

## Binomial test:

binom.test(c(sum(IRR_both_step4$Sam_figop == IRR_both_step4$Paula_figop),
	sum(IRR_both_step4$Sam_figop != IRR_both_step4$Paula_figop)), p = 1 / 6)

binom.test(c(sum(IRR_both_step5$Sam_figop == IRR_both_step5$Paula_figop),
	sum(IRR_both_step5$Sam_figop != IRR_both_step5$Paula_figop)), p = 1 / 6)



