
p <- runif(999999, 0.0000000001, 0.9999999999)
profvis({			
  log(p/(1-p))			
  -log(1/p-1)	
  logit.2(p)
  logit.1(p)
  qlogis(p)
})
