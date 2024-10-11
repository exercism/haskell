module FreelancerRates (dailyRate, applyDiscount, monthlyRate, daysInBudget) where

dailyRate :: Double -> Double 
dailyRate = error "Implement this function."

applyDiscount :: Double -> Double -> Double 
applyDiscount amount disscount = amount * (1 - disscount / 100)

monthlyRate :: Double -> Double -> Int
monthlyRate hourlyRate discount = ceiling (applyDiscount (dailyRate hourlyRate * 22) discount)

daysInBudget :: Int -> Double -> Double -> Double
daysInBudget budget hourlyRate discount = 
    fromIntegral (floor (fromIntegral budget / applyDiscount (dailyRate hourlyRate) discount * 10)) / 10
