diri4 <- diriD(3400,48,29,17)
a_1_4 <- diri4$a1
a_2_4 <- diri4$a2
a_3_4 <- diri4$a3
a_4_4 <- diri4$a4
pplot(plotname="OUTPUTS/alphaxof4a.tiff",lst=plotlist("a1 desc"=a_1_4,a_2_4))
pplot(plotname="OUTPUTS/alphaxof4b.tiff",filter="a_?_4")

ga_2_4 <- GlobalAlpha(2,diri4)
ga_3_4 <- GlobalAlpha(3,diri4)
ga_4_4 <- GlobalAlpha(4,diri4)
pplot(plotname="OUTPUTS/globalalphaxof4.tiff",filter="ga_?_4")

XYZ <- c(2600,46,28)
#diri3 <- diriD(2600,46,28)
diri3 <- diriD(c(2600,46,28))
a_1_3 <- diri3$a1
a_2_3 <- diri3$a2
a_3_3 <- diri3$a3
pplot(plotname="OUTPUTS/alphaxof3.tiff",filter="a_?_3")

ga_2_3 <- GlobalAlpha(2,diri3)
ga_3_3 <- GlobalAlpha(3,diri3)
pplot(plotname="OUTPUTS/globalalphaxof3.tiff",filter="ga_?_3")
