RS25_eta <- lognD(median=484726900,EF=5)
RS25_lambda <- lognD(median=3e-7,EF=5)
RS25_lambdaTI <- RS25_lambda
RS25_lambdaTS <- RS25_lambda
RS25_RF <- lognD(median=22,EF=1.5)
scatterbar(plotname="OUTPUTS/RS25.tiff",filter="RS25_[elR]*")
