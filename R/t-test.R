power.ttest <- function(alpha=0.05,power=0.8,diff=3.3,alternative=2,sigma=5.1,type=1)
{
# alpha : type I error
# power : 1 - type II error
# diff : mean difference
# alternative : 1 = one sided , 2 = two sided
# sigma : standard deviation
# type : 1 = one sample or paired sample, 2 = two samples

	old = 2
	for (i in 1:100)
	{
		new = type*(qt(1-alpha/alternative,type*(old-1))+qt(power,type*(old-1)))^2 * sigma^2/ diff^2
		if(abs(new-old) > 0.1e-16)
			old = new
		else
			break
		#cat("num[",i,"]=", new, "\n")
	}
	list(n=new)
}