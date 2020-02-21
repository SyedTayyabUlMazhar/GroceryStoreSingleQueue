findQueueWaitTime <- function(customerNumber) {
  if (customerNumber == 1)
    0
  else{
    if (serviceEnd[customerNumber - 1] > arrivalTimes[customerNumber])
      serviceEnd[customerNumber - 1] - arrivalTimes[customerNumber]
    else
      0
  }
}

findServiceBeginTime <- function(customerNumber) {
  if (customerNumber == 1)
    0
  else{
    arrivalTimes[customerNumber] + queueWaitTime[customerNumber]
  }
}

findServiceEndTime <- function(customerNumber) {
  serviceBegin[customerNumber] + serviceTimes[customerNumber]
}

findTimeCustomerSpendsInSystem <- function(customerNumber) {
  serviceTimes[customerNumber] + queueWaitTime[customerNumber]
}

findIdleTimeOfServer <- function(customerNumber) {
  if (customerNumber == 1 ||
      !arrivalTimes[customerNumber] > serviceEnd[customerNumber - 1])
    0
  else
    arrivalTimes[customerNumber] - serviceEnd[customerNumber - 1]
}
# interArrivalTimes = c(0,8,6,1,8,3,8,7,2,3)
# serviceTimes = c(4,1,4,3,2,4,5,4,5,3)
interArrivalTimes <- c(0)
serviceTimes <- c()
arrivalTimes <- c(0)
queueWaitTime <- c()
serviceBegin <- c()
serviceEnd <- c()
timeCustomerSpendInSystem <- c()
serverIdleTime <- c()

repeat {
  numberOfCustomers <-
    readline("Enter number of customers to simulate for : ")
  if (!is.na(as.integer(numberOfCustomers))) {
    numberOfCustomers <- as.integer(numberOfCustomers)
    break
  }
}


# Generate random interArrival and ServiceTimes.
interArrivalTimes <-
  append(interArrivalTimes, floor(runif(numberOfCustomers - 1, 1, 9)))
serviceTimes <-
  sample(
    c(1, 2, 3, 4, 5, 6),
    size = numberOfCustomers,
    replace = TRUE,
    prob = c(0.1, 0.2, 0.3, 0.25, 0.1, 0.05)
  )


#calculate arrival times
if (numberOfCustomers >= 2)
  for (i in 2:numberOfCustomers) {
    arrivalTimes[i] <- interArrivalTimes[i] + arrivalTimes[i - 1]
  }

for (i in 1:numberOfCustomers)
{
  queueWaitTime <- append(queueWaitTime, findQueueWaitTime(i))
  serviceBegin <- append(serviceBegin, findServiceBeginTime(i))
  serviceEnd <- append(serviceEnd, findServiceEndTime(i))
  timeCustomerSpendInSystem <-
    append(timeCustomerSpendInSystem,
           findTimeCustomerSpendsInSystem(i))
  serverIdleTime <- append(serverIdleTime, findIdleTimeOfServer(i))
}

# print(interArrivalTimes)
# print(serviceTimes)
# print(arrivalTimes)
# print(queueWaitTime)
# print(serviceBegin)
# print(serviceEnd)
# print(timeCustomerSpendInSystem)
# print(serverIdleTime)


totalServiceTime <- sum(serviceTimes)
avgServiceTimes <- mean(serviceTimes)
totalQueueWaitTime <-sum(queueWaitTime)

queueWaitProbability <-
  (length(which(queueWaitTime != 0))) / numberOfCustomers
serverBusyPercentage <-
  sum(serverIdleTime) / serviceEnd[numberOfCustomers]
avgTimeBetweenArrivals <- sum(interArrivalTimes)/(numberOfCustomers-1)

avgQueueWaitTime <- sum(queueWaitTime)/(length(which(queueWaitTime!=0)))
avgTimeCustomerSpendsInSystem <- sum(timeCustomerSpendInSystem)/numberOfCustomers

print(df)
print("********************* R E S U L T ***************************")
print(
  cat(
    "Total Service Time:",
    totalServiceTime,
    " minutes,"
    ,
    "Avg. Service Time: ",
    avgServiceTimes,
    "\nTotal Queue wait Time: ",
    sum(queueWaitTime),
    "Probabilty that a customer has to wait:",
    queueWaitProbability,
    "\nThe probabilistic percentage of total time that a server will be busy : ",
    serverBusyPercentage,
    "\nAvg Time Between Arrivals:",
    avgTimeBetweenArrivals,
    "\nAvg. Queue wait time:",
    avgQueueWaitTime,
    "\nAvg. time customer spends in system:",
    avgTimeCustomerSpendsInSystem
  )
)
print("*************************************************************")

dfSimulation = data.frame(
  interArrivalTimes,
  arrivalTimes,
  serviceTimes,
  queueWaitTime,
  serviceBegin,
  serviceEnd,
  timeCustomerSpendInSystem,
  serverIdleTime
)


dfResult = data.frame(
  
  totalServiceTime,
  avgServiceTimes,
  sum(queueWaitTime),
  queueWaitProbability,
  serverBusyPercentage,
  avgTimeBetweenArrivals,
  avgQueueWaitTime,
  avgTimeCustomerSpendsInSystem
  
)
write.xlsx(dfSimulation, paste(getwd(), "output.xlsx", sep = "/"), sheetName="Simulation")
write.xlsx(dfResult, paste(getwd(), "output.xlsx", sep = "/"), sheetName="Result", append = TRUE)