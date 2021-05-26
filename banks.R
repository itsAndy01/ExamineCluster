#Step 2: 
banks.df<- read.csv('banks.csv')
View(banks.df)
# Step 3: Run the logistics regression model 
lgreg <- glm(Financial.Condition~ TotExp.Assets + TotLns.Lses.Assets, 
              data = banks.df, family='binomial')
#Step 4: 
summary(lgreg)

#Step 5:
      #logit = -14.721 + (89.834*TotExp.Assets)+(8.371*TotLns.Lses.Assets)
      #odds = (exp(-14.721 + (89.834 * TotExp.Assets)+ (8.371*TotLns.Lses.Assets)))
      
      #Probability 
      #p(choice = 1) = e^(-14.721 + (89.834 * TotExp.Assets) + (8.371*TotLns.Lses.Assets))/(1+ e^(-14.721 + (89.834 * TotExp.Assets)+ (8.371*TotLns.Lses.Assets)))
      #a helpful formula for probability is = (odds)/(1+odds)

#Step 6: Get the predictions
lgreg.pred <- predict(lgreg, data=banks.df, type = 'response')

#Step 7: calculate the logit, odds and probability for a bank that has 0.6 and 0.11
TotExp.Assets.STEP <- .11
TotLns.Lses.Assets.STEP <- .6

      logit <- -14.721 + (89.834*TotExp.Assets.STEP)+(8.371*TotLns.Lses.Assets.STEP)
      #logit= 0.1833
      odds <- (exp(-14.721 + (89.834 * TotExp.Assets.STEP)+ (8.371*TotLns.Lses.Assets.STEP)))
      #odds= 1.2012
      #probability: 
      probability = (odds)/(1+odds)
      # probability= 0.5457
#Since prob greater than .5, then the bank is weak or 1
#lower .5 means it is strong
      
#Step 8:
    #logit =
    log(0.5/(1-0.5))
    #logit = 0
    
    #odds = 
    (0.5/(1-0.5))
    #odds= 1

#Step 9: Interpretation of TotLns...

#Given Everything else is the same, 1 unit increase in TotLns.Lses.Assets will increase the odds of the bank being financially weak by 
#e^(0.1*8.371).
#logit will only have the 0.1 increase linear wise 
    