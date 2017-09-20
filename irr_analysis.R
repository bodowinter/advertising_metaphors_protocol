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
IRR2[IRR2$Paula == 'Phone, you', ]$Paula <- 'Phone'
IRR2[IRR2$Sam_figop == 'Phone, text', ]$Sam_figop <- 'Phone'
IRR2[IRR2$Paula == 'Phone, text', ]$Paula <- 'Phone'
IRR2[IRR2$Sam_figop == 'Siri and iPhone', ]$Sam_figop <- 'iPhone'
IRR2[IRR2$Paula == 'Siri and iPhone', ]$Paula <- 'iPhone'
IRR2[IRR2$Sam_figop == 'OM4G ', ]$Sam_figop <- 'OM4G'
IRR2[IRR2$Paula == 'OM4G ', ]$Paula <- 'OM4G'
IRR2[IRR2$Sam_figop == '4G ', ]$Sam_figop <- '4G'
IRR2[IRR2$Paula == '4G ', ]$Paula <- '4G'
IRR2[IRR2$Sam_figop == 'Tesco Mobile as a serious contender in competing mobile network companies', ]$Sam_figop <- 'Tesco Mobile '
IRR2[IRR2$Paula == 'Tesco gets you a Samsung Galaxy and a good service on any network', ]$Paula <- 'Tesco Mobile '

## Get subsets:

step1 <- filter(IRR2, Step == 1) %>%
	select(Sam_figop, Paula)
step2 <- filter(IRR2, Step == 2) %>%
	select(Sam_figop, Paula)
step3 <- filter(IRR2, Step == 3) %>%
	select(Sam_figop, Paula)
step4 <- filter(IRR2, Step == 4) %>%
	select(Sam_figop, Paula)
step5 <- filter(IRR2, Step == 5) %>%
	select(Sam_figop, Paula)

## Perform IRRs:

kappam.fleiss(step1)
kappam.fleiss(step2)
kappam.fleiss(step3)
kappam.fleiss(step4)
kappam.fleiss(step5)



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

## Get subsets:

step1 <- filter(IRR3, Step == 1) %>%
	select(Sam_figop, Paula_figop)
step2 <- filter(IRR3, Step == 2) %>%
	select(Sam_figop, Paula_figop)
step3 <- filter(IRR3, Step == 3) %>%
	select(Sam_figop, Paula_figop)
step4 <- filter(IRR3, Step == 4) %>%
	select(Sam_figop, Paula_figop)
step5 <- filter(IRR3, Step == 5) %>%
	select(Sam_figop, Paula_figop)

## Perform IRRs:

kappam.fleiss(step1)
kappam.fleiss(step2)
kappam.fleiss(step3)
kappam.fleiss(step4)
kappam.fleiss(step5)


