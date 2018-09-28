#what is the relationship between LY from terra and population? plot the midpoint of distance by base to find out

distance <- c(250, 550, 675, 875, 1125, 1625)
base_pop_high_sl <- c(500*10^6,100*10^6,25*10^6,5*10^6,1*10^6,2*10^5) 
base_pop_low_sl <- c(50*10^6,10*10^6,2.5*10^6,5*10^5,1*10^5,1*10^4)
#for recent settlement, I needed to tweak numbers a bit to fit decently with a spline
#I halved the 500-600 LY numbers, and doubled the numbers from 500-600 and 750-1000. I also doubled the number 
#for 600-750 low roll again, because that original number (50,000) was particularly low
base_pop_high_recent <- c(1*10^5, 10*10^6, 2*10^6, 4*10^5, 5*10^4, 1*10^4)
base_pop_low_recent <- c(1*10^4, 1*10^6, 2*10^5, 4*10^4, 5*10^3, 500)

plot(distance, base_pop_high_sl)
model_sl_high <- lm(log(base_pop_high_sl)~distance)
lines(distance, exp(model_sl_high$fitted.values), lwd=3, col="red")

#out of sample predicctions
predicted_base_high_sl <-  exp(predict(model_sl_high, data.frame(distance=0:2500)))
plot(0:2500, predicted_base_high_sl, type="l")
points(distance, base_pop_high_sl, pch=21, bg="red", cex=2)


plot(distance, base_pop_low_sl)
model_sl_low <- lm(log(base_pop_low_sl)~distance)
lines(distance, exp(model_sl_low$fitted.values), lwd=3, col="red")

#out of sample predicctions
predicted_base_low_sl <-  exp(predict(model_sl_low, data.frame(distance=0:2500)))
plot(0:2500, predicted_base_low_sl, type="l")
points(distance, base_pop_low_sl, pch=21, bg="red", cex=2)


plot(distance, base_pop_high_recent)
#spline of distance between 0 and 500?
before_spline <- distance-550
before_spline[before_spline>0] <- 0
after_spline <- distance-550
after_spline[after_spline<0] <- 0
model_recent_high <- lm(log(base_pop_high_recent)~before_spline+after_spline, weights=c(1,40,1,1,1,1))
lines(distance, exp(model_recent_high$fitted.values), lwd=3, col="red")

#out of sample predicctions
temp <- data.frame(distance=0:2500)
temp$before_spline <- temp$distance-550
temp$before_spline[temp$before_spline>0] <- 0
temp$after_spline <- temp$distance-550
temp$after_spline[temp$after_spline<0] <- 0
predicted_base_high_recent <-  exp(predict(model_recent_high, temp))
plot(0:2500, predicted_base_high_recent, type="l", ylim=c(0, max(base_pop_high_recent)))
points(distance, base_pop_high_recent, pch=21, bg="red", cex=2)


plot(distance, base_pop_low_recent)
model_recent_low <- lm(log(base_pop_low_recent)~before_spline+after_spline, weights=c(1,40,1,1,1,1))
lines(distance, exp(model_recent_low$fitted.values), lwd=3, col="red")
predicted_base_low_recent <-  exp(predict(model_recent_low, temp))
plot(0:2500, predicted_base_low_recent, type="l", ylim=c(0, max(base_pop_low_recent)))
points(distance, base_pop_low_recent, pch=21, bg="red", cex=2)

#these look pretty good. I think we can use these equations rather than the table to estimate base populations for each 
#of our categories. 