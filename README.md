# customer-analytics
Customer analytics is about using customer data to make business decisions and predict future behavior through building and implementing powerful models for customer acquisition, retention, behavioral patterns such as website visits, customer lifetime value and direct marketing responses. 

title: "Final Project"
author: "Ruyi Ding"
date: "December 3, 2015"
output: pdf_document
---
```{r}
rm(list = ls());
library(bayesm);
library(m537);
library(m537tools);
library(xts);
```

Part 1. Data Preperation
```{r}
data(tuna);
n = dim(tuna)[1];
tuna_training=tuna[1:300,]
tuna_test=tuna[(n-37):n,]

customers = tuna_training[,30]
sales = tuna_training[,2:8] 
share=sales/mean(customers) 
shareout=as.numeric(1-rowSums(share)) 
logshareratio=log(share/shareout)

dts = seq(from = as.Date("2000-01-07"),
          by = "weeks",
          length.out = 300)
```


Part 2. Prior Selection & Gaussian Model
```{r}
# Star Kist 6 oz
dts = seq(from = as.Date("2000-01-07"),
          by = "weeks",
          length.out = 300)

lshareratiosk = logshareratio[,1];
lpricesk = tuna_training[,"LPRICE1"];
dispsk = tuna_training[,"NSALE1"];

lshareratioskxts = xts(lshareratiosk,
                   order.by = dts)
lpriceskxts = xts(lpricesk,
                  order.by = dts)

datskdf = data.frame(lshareratiosk = lshareratioskxts,
                     lpricesk = lpriceskxts,
                     dispsk = dispsk);


beta0_ = c(-6,-4,0);
B0v_ = diag(.5,3);
s20_ = .5;
s2v_ = .5;

tunasksp = samplethepriorregm(lshareratiosk~lpricesk+dispsk,
                             datdf = datskdf,
                             beta0_ = beta0_,
                             B0v_ = B0v_,
                             s20_ = s20_,
                             s2v_ = s2v_); 

prob = c(.05,.25,.5,.75,.95);
tunaskspq = apply(tunasksp,1,"quantile",prob);
t(tunaskspq)

tunasklpd = MCMCregressg(lshareratiosk~lpricesk+dispsk,
                       data = datskdf, 
                       beta0_ = beta0_,
                       B0_ = B0v_,
                       s20_ = s20_,
                       s2v = s2v_)

summarymcmc(tunasklpd)

# Chicken of the Sea 6 oz
lshareratiocs = logshareratio[,2];
lpricecs = tuna_training[,"LPRICE2"];
dispcs = tuna_training[,"NSALE2"];

lshareratiocsxts = xts(lshareratiocs,
                       order.by = dts)
lpricecsxts = xts(lpricecs,
                  order.by = dts)

datcsdf = data.frame(lshareratiocs = lshareratiocsxts,
                     lpricecs = lpricecsxts,
                     dispcs = dispcs);


beta0_ = c(-6,-4,.5);
B0v_ = diag(.5,3);
s20_ = .5;
s2v_ = .5;

tunacssp = samplethepriorregm(lshareratiocs~lpricecs+dispcs,
                              datdf = datcsdf,
                              beta0_ = beta0_,
                              B0v_ = B0v_,
                              s20_ = s20_,
                              s2v_ = s2v_); 

prob = c(.05,.25,.5,.75,.95);
tunacsspq = apply(tunacssp,1,"quantile",prob);
t(tunacsspq)

tunacslpd = MCMCregressg(lshareratiocs~lpricecs+dispcs,
                         data = datcsdf, 
                         beta0_ = beta0_,
                         B0_ = B0v_,
                         s20_ = s20_,
                         s2v = s2v_)

summarymcmc(tunacslpd)

# Bumble Bee Solid 6.12 oz
lshareratiobbs = logshareratio[,3];
lpricebbs = tuna_training[,"LPRICE3"];
dispbbs = tuna_training[,"NSALE3"];

lshareratiobbsxts = xts(lshareratiobbs,
                       order.by = dts)
lpricebbsxts = xts(lpricebbs,
                  order.by = dts)

datbbsdf = data.frame(lshareratiobbs = lshareratiobbsxts,
                     lpricebbs = lpricebbsxts,
                     dispbbs = dispbbs);


beta0_ = c(-5.5,-2.5,0.5);
B0v_ = diag(.5,3);
s20_ = 1;
s2v_ = .5;

tunabbssp = samplethepriorregm(lshareratiobbs~lpricebbs+dispbbs,
                              datdf = datbbsdf,
                              beta0_ = beta0_,
                              B0v_ = B0v_,
                              s20_ = s20_,
                              s2v_ = s2v_); 

prob = c(.05,.25,.5,.75,.95);
tunabbsspq = apply(tunabbssp,1,"quantile",prob);
t(tunabbsspq)

tunabbslpd = MCMCregressg(lshareratiobbs~lpricebbs+dispbbs,
                         data = datbbsdf, 
                         beta0_ = beta0_,
                         B0_ = B0v_,
                         s20_ = s20_,
                         s2v = s2v_)

summarymcmc(tunabbslpd)

# Bumble Bee Chunk 6.12 oz
lshareratiobbc = logshareratio[,4];
lpricebbc = tuna_training[,"LPRICE4"];
dispbbc = tuna_training[,"NSALE4"];

lshareratiobbcxts = xts(lshareratiobbc,
                        order.by = dts)
lpricebbcxts = xts(lpricebbc,
                   order.by = dts)

datbbcdf = data.frame(lshareratiobbc = lshareratiobbcxts,
                     lpricebbc = lpricebbcxts,
                     dispbbc = dispbbc);


beta0_ = c(-6.5,-4.5,0);
B0v_ = diag(.5,3);
s20_ = .5;
s2v_ = .5;

tunabbcsp = samplethepriorregm(lshareratiobbc~lpricebbc+dispbbc,
                               datdf = datbbcdf,
                               beta0_ = beta0_,
                               B0v_ = B0v_,
                               s20_ = s20_,
                               s2v_ = s2v_); 

prob = c(.05,.25,.5,.75,.95);
tunabbcspq = apply(tunabbcsp,1,"quantile",prob);
t(tunabbcspq)

tunabbclpd = MCMCregressg(lshareratiobbc~lpricebbc+dispbbc,
                          data = datbbcdf, 
                          beta0_ = beta0_,
                          B0_ = B0v_,
                          s20_ = s20_,
                          s2v = s2v_)

summarymcmc(tunabbclpd)

# Geisha 6 oz
lshareratiogei = logshareratio[,5];
lpricegei = tuna_training[,"LPRICE5"];
dispgei = tuna_training[,"NSALE5"];

lshareratiogeixts = xts(lshareratiogei,
                        order.by = dts)
lpricegeixts = xts(lpricegei,
                   order.by = dts)

datgeidf = data.frame(lshareratiogei = lshareratiogeixts,
                      lpricegei = lpricegeixts,
                      dispgei = dispgei);


beta0_ = c(-5,-4,0);
B0v_ = diag(.5,3);
s20_ = .5;
s2v_ = .5;

tunageisp = samplethepriorregm(lshareratiogei~lpricegei+dispgei,
                              datdf = datgeidf,
                              beta0_ = beta0_,
                              B0v_ = B0v_,
                              s20_ = s20_,
                              s2v_ = s2v_); 

prob = c(.05,.25,.5,.75,.95);
tunageispq = apply(tunageisp,1,"quantile",prob);
t(tunageispq)

tunageilpd = MCMCregressg(lshareratiogei~lpricegei+dispgei,
                         data = datgeidf, 
                         beta0_ = beta0_,
                         B0_ = B0v_,
                         s20_ = s20_,
                         s2v = s2v_)

summarymcmc(tunageilpd)

# Bumble Bee Large Cans
lshareratiobblc = logshareratio[,6];
lpricebblc = tuna_training[,"LPRICE6"];
dispbblc = tuna_training[,"NSALE6"];

lshareratiobblcxts = xts(lshareratiobblc,
                        order.by = dts)
lpricebblcxts = xts(lpricebblc,
                   order.by = dts)

datbblcdf = data.frame(lshareratiobblc = lshareratiobblcxts,
                       lpricebblc = lpricebblcxts,
                       dispbblc = dispbblc);

beta0_ = c(-11,3,1);
B0v_ = diag(1,3);
s20_ = .5;
s2v_ = .5;

tunabblcsp = samplethepriorregm(lshareratiobblc~lpricebblc+dispbblc,
                               datdf = datbblcdf,
                               beta0_ = beta0_,
                               B0v_ = B0v_,
                               s20_ = s20_,
                               s2v_ = s2v_); 

prob = c(.05,.25,.5,.75,.95);
tunabblcspq = apply(tunabblcsp,1,"quantile",prob);
t(tunabblcspq)

tunabblclpd = MCMCregressg(lshareratiobblc~lpricebblc+dispbblc,
                          data = datbblcdf, 
                          beta0_ = beta0_,
                          B0_ = B0v_,
                          s20_ = s20_,
                          s2v = s2v_)

summarymcmc(tunabblclpd)


# HH Chunk Lite 6.5 oz
lshareratiohh = logshareratio[,7];
lpricehh = tuna_training[,"LPRICE6"];
disphh = tuna_training[,"NSALE6"];

lshareratiohhxts = xts(lshareratiohh,
                         order.by = dts)
lpricehhxts = xts(lpricehh,
                    order.by = dts)

dathhdf = data.frame(lshareratiohh = lshareratiohhxts,
                     lpricehh = lpricehhxts,
                     disphh = disphh);

beta0_ = c(-1,-4,0);
B0v_ = diag(1,3);
s20_ = 1;
s2v_ = .5;

tunahhsp = samplethepriorregm(lshareratiohh~lpricehh+disphh,
                              datdf = dathhdf,
                              beta0_ = beta0_,
                              B0v_ = B0v_,
                              s20_ = s20_,
                              s2v_ = s2v_); 

prob = c(.05,.25,.5,.75,.95);
tunahhspq = apply(tunahhsp,1,"quantile",prob);
t(tunahhspq)

tunahhlpd = MCMCregressg(lshareratiohh~lpricehh+disphh,
                         data = dathhdf, 
                         beta0_ = beta0_,
                         B0_ = B0v_,
                         s20_ = s20_,
                         s2v = s2v_)

summarymcmc(tunahhlpd)

log=logmarglik(list(tunahhlpd,tunabblclpd,tunageilpd,tunabbslpd,tunasklpd,tunacslpd,tunabbclpd))
sum(log)
```

Part 3 SURE Model
```{r}
datdf = cbind(datskdf,datcsdf,datbbsdf,datbbcdf,datgeidf,datbblcdf,dathhdf)

modelfrmls = list(lshareratiosk~lpricesk+dispsk,
                  lshareratiocs~lpricecs+dispcs,
                  lshareratiobbs~lpricebbs+dispbbs,
                  lshareratiobbc~lpricebbc+dispbbc,
                  lshareratiogei~lpricegei+dispgei,
                  lshareratiobblc~lpricebblc+dispbblc,
                  lshareratiohh~lpricehh+disphh)

beta0_ = c(-6,-4,0,-6,-4,0.5,-5.5,-2.5,0.5,-6.5,-4.5,0,-5,-4,0,-11,3,1,-1,-4,0)
B0v_ = diag(1,21);
ommean = diag(7);
rho = 13;

tunasuresp = samplethepriorsureg(modelfrmls = modelfrmls,
                                 data  = datdf,
                                 beta0_ = beta0_,
                                 B0_ = B0v_,
                                 rho = rho,
                                 ommean = ommean)
prob = c(.05,.25,.5,.75,.95);
tunabblcspq = apply(tunabblcsp,1,"quantile",prob);
t(tunabblcspq)

tunasurespr=list(StarKist=0,
                 ChickenoftheSea=0,
                 BumbleBeeSolid=0,
                 BumbleBeeChunk=0,
                 Geisha=0,
                 BumbleBeeLargeCans=0,
                 HHChunkLite=0)
for(i in 1:7){
    tunasurespr[[i]]=matrix(0,nr=10,nc=5)
    colnames(tunasurespr[[i]])=c("5%","25%","50%","75%","95%")
    rownames(tunasurespr[[i]])=rep(0,length(tunasuresp))
    for(j in 1:length(tunasuresp)){
            rownames(tunasurespr[[i]])[j]=unique(rownames(tunasuresp[[j]]))
}}

for(i in 1:7){
    for(j in 1:10){
        tunasurespr[[i]][j,]=t(apply(tunasuresp[[j]],1,"quantile",prob))[i,]
    }
}

thetam = MCMCsureg(modelfrmls = modelfrmls,
                   data  = datdf,
                   beta0_ = beta0_,
                   B0_ = B0v_,
                   rho = rho,
                   ommean = ommean);

summarymcmc(thetam)
```

Part 4 Prediction Performance
```{r}
customers = tuna_test[,30]
sales = tuna_test[,2:8] 
share =sales/mean(customers) 
shareout =as.numeric(1-rowSums(share)) 
logshareratio=log(share/shareout)

dts = seq(from = as.Date("2005-10-07"),
          by = "weeks",
          length.out = 38)

lshareratiosk = logshareratio[,1];
lpricesk = tuna_test[,"LPRICE1"];
dispsk = tuna_test[,"NSALE1"];

lshareratioskxts = xts(lshareratiosk,
                       order.by = dts)
lpriceskxts = xts(lpricesk,
                  order.by = dts)

datskdf = data.frame(lshareratiosk = lshareratioskxts,
                     lpricesk = lpriceskxts,
                     dispsk = dispsk);

lshareratiocs = logshareratio[,2];
lpricecs = tuna_test[,"LPRICE2"];
dispcs = tuna_test[,"NSALE2"];

lshareratiocsxts = xts(lshareratiocs,
                       order.by = dts)
lpricecsxts = xts(lpricecs,
                  order.by = dts)

datcsdf = data.frame(lshareratiocs = lshareratiocsxts,
                     lpricecs = lpricecsxts,
                     dispcs = dispcs);

lshareratiobbs = logshareratio[,3];
lpricebbs = tuna_test[,"LPRICE3"];
dispbbs = tuna_test[,"NSALE3"];

lshareratiobbsxts = xts(lshareratiobbs,
                        order.by = dts)
lpricebbsxts = xts(lpricebbs,
                   order.by = dts)

datbbsdf = data.frame(lshareratiobbs = lshareratiobbsxts,
                      lpricebbs = lpricebbsxts,
                      dispbbs = dispbbs);

lshareratiobbc = logshareratio[,4];
lpricebbc = tuna_test[,"LPRICE4"];
dispbbc = tuna_test[,"NSALE4"];

lshareratiobbcxts = xts(lshareratiobbc,
                        order.by = dts)
lpricebbcxts = xts(lpricebbc,
                   order.by = dts)

datbbcdf = data.frame(lshareratiobbc = lshareratiobbcxts,
                      lpricebbc = lpricebbcxts,
                      dispbbc = dispbbc);

lshareratiogei = logshareratio[,5];
lpricegei = tuna_test[,"LPRICE5"];
dispgei = tuna_test[,"NSALE5"];

lshareratiogeixts = xts(lshareratiogei,
                        order.by = dts)
lpricegeixts = xts(lpricegei,
                   order.by = dts)

datgeidf = data.frame(lshareratiogei = lshareratiogeixts,
                      lpricegei = lpricegeixts,
                      dispgei = dispgei);

lshareratiobblc = logshareratio[,6];
lpricebblc = tuna_test[,"LPRICE6"];
dispbblc = tuna_test[,"NSALE6"];

lshareratiobblcxts = xts(lshareratiobblc,
                         order.by = dts)
lpricebblcxts = xts(lpricebblc,
                    order.by = dts)

datbblcdf = data.frame(lshareratiobblc = lshareratiobblcxts,
                       lpricebblc = lpricebblcxts,
                       dispbblc = dispbblc);

lshareratiohh = logshareratio[,7];
lpricehh = tuna_test[,"LPRICE6"];
disphh = tuna_test[,"NSALE6"];

lshareratiohhxts = xts(lshareratiohh,
                       order.by = dts)
lpricehhxts = xts(lpricehh,
                  order.by = dts)

dathhdf = data.frame(lshareratiohh = lshareratiohhxts,
                     lpricehh = lpricehhxts,
                     disphh = disphh);

datdftest = cbind(datskdf,datcsdf,datbbsdf,datbbcdf,datgeidf,datbblcdf,dathhdf)

# SURE Model
tunapred=predictsureg(thetam=thetam,
                      pdatdf = datdftest,
                      logr = F)

yname=attr(thetam,"yname")

T=38
MSPEsure=matrix(0,nr=7,nc=1)
colnames(MSPEsure)=c("MSPE")
rownames(MSPEsure)=c("Star Kist",
                     "Chicken of the Sea",
                     "Bumble Bee Solid",
                     "Bumble Bee Chunk",
                     "Geisha",
                     "Bumble Bee Large Cans",
                     "HH Chunk Lite")

head(datdftest)

for(i in 1:dim(MSPEsure)[1]){
    for(j in 1:length(tunapred)){
        MSPEsure[i,]= MSPEsure[i,]+(mean(tunapred[[j]][i,])-datdftest[j,i*3-2])^2
        }
    }
MSPEsure
sum(MSPEsure)/T


#  Gaussian model
predictg=function(thetam = thetam, pdatdf = pdatdf, logr = FALSE, seed = 100) 
{
    modelfrm = attr(thetam, "modelfrm")
    yname = attr(thetam, "yname")
    k1 = dim(thetam)[2]
    k = k1 - 1
    betam = t(thetam[, 1:k])
    s2m = thetam[, k1]
    m = dim(betam)[2]
    if (class(pdatdf) != "data.frame") {
        stop(cat("pdatdf is not a data.frame", "\n"))
    }
    Xf = model.matrix(modelfrm, pdatdf)
    a = pdatdf[, yname]
    if (logr) {
        a = exp(a)
    }
    h = dim(Xf)[1]
    indf = as.Date(rownames(Xf))
    yfm = matrix(0, nr = h, nc = m)
    g = 1
    while (g <= m) {
        beta = betam[, g]
        s2 = s2m[g]
        yf = Xf %*% beta + sqrt(s2) * rnorm(h)
        if (logr) {
            yfm[, g] = exp(yf)
        }
        else {
            yfm[, g] = yf
        }
        g = g + 1
    }
    yfhat = apply(yfm, 1, "mean")
    yfhatxts = xts(yfhat, order.by = indf)
    
    return(yfhatxts)
}

tunaskpred = predictg(tunasklpd,
                      datskdf,
                      logr = F)

tunacspred = predictg(tunacslpd,
                      datcsdf,
                      logr = F)

tunabbspred = predictg(tunabbslpd,
                      datbbsdf,
                      logr = F)

tunabbcpred = predictg(tunabbclpd,
                       datbbcdf,
                       logr = F)

tunageipred = predictg(tunageilpd,
                       datgeidf,
                       logr = F)

tunabblcpred = predictg(tunabblclpd,
                        datbblcdf,
                        logr = F)

tunahhpred = predictg(tunahhlpd,
                      dathhdf,
                      logr = F)

T=38
MSPEg=matrix(0,nr=7,nc=1)
colnames(MSPEg)=c("MSPE")
rownames(MSPEg)=c("Star Kist",
                     "Chicken of the Sea",
                     "Bumble Bee Solid",
                     "Bumble Bee Chunk",
                     "Geisha",
                     "Bumble Bee Large Cans",
                     "HH Chunk Lite")

for(i in 1:length(tunaskpred)){
    MSPEg[1,]= MSPEg[1,]+(tunaskpred[i,]-datdftest[i,1])^2
    }

for(i in 1:length(tunacspred)){
    MSPEg[2,]= MSPEg[2,]+(tunacspred[i,]-datdftest[i,2*3-2])^2
}

for(i in 1:length(tunabbspred)){
    MSPEg[3,]= MSPEg[3,]+(tunabbspred[i,]-datdftest[i,3*3-2])^2
}

for(i in 1:length(tunabbcpred)){
    MSPEg[4,]= MSPEg[4,]+(tunabbcpred[i,]-datdftest[i,4*3-2])^2
}

for(i in 1:length(tunageipred)){
    MSPEg[5,]= MSPEg[5,]+(tunageipred[i,]-datdftest[i,5*3-2])^2
}

for(i in 1:length(tunabblcpred)){
    MSPEg[6,]= MSPEg[6,]+(tunabblcpred[i,]-datdftest[i,6*3-2])^2
}

for(i in 1:length(tunahhpred)){
    MSPEg[7,]= MSPEg[7,]+(tunahhpred[i,]-datdftest[i,7*3-2])^2
}

MSPEg
sum(MSPEg)/T
```

Part 5 Price Simulation
```{r}
lprice1 = c(log(0.44),log(0.8),log(1.75),log(0.8),
            log(1.45),log(3.39),log(0.75))

lprice2 = c(log(0.54),log(0.8),log(1.75),log(0.8),
            log(1.45),log(3.39),log(0.75))

lprice3 = c(log(0.64),log(0.8),log(1.75),log(0.8),
            log(1.45),log(3.39),log(0.75))

lprice4 = c(log(0.74),log(0.8),log(1.75),log(0.8),
            log(1.45),log(3.39),log(0.75))

lprice5 = c(log(0.84),log(0.8),log(1.75),log(0.8),
            log(1.45),log(3.39),log(0.75))

display=c(0.31,0.35,0.29,0.23,0.35,0.25,0.24)

prddf= datdftest[38,]


salesratio = function(lprice = lprice,
                      display=display,
                      prddf = prddf, 
                      thetam = thetam,
                      seed = 123){
    set.seed(seed);
    ks = attr(thetam, "k")
    dd = attr(thetam, "dd")
    m = dim(thetam)[1];
    modelfrmls = attr(thetam,"modelfrmls");
    xnamels = attr(thetam,"xnamels");
    d = length(xnamels);
    yfm = matrix(0, nr = d, nc = m);
    outls = suremat(modelfrmls, prddf);
    Xf = outls$Xs
    blength = 0;
    for(i in 1:d){
        xnamelsi = xnamels[[i]];
        pid = grep("price",xnamelsi);
        Xf[i,(blength+pid)]=lprice[i];
        blength = blength + length(xnamelsi)
    }
    blength = 0;
    for(i in 1:d){
        xnamelsi = xnamels[[i]];
        pid = grep("disp",xnamelsi);
        Xf[i,(blength+pid)]=display[i];
        blength = blength + length(xnamelsi)
    }
    for (g in 1:m) {
        thetag = thetam[g, ]
        betag = thetag[1:ks]
        omg = thetag[(ks + 1):(ks + dd)]
        Omegag = xpnd(omg)
        Cg = t(chol(Omegag))
        logshareratio = Xf %*% betag + Cg %*% rnorm(d);
        yfm[, g] = logshareratio; # yfm=[d,m]
    }
    salesratio=exp(mean(yfm[1,])-mean(yfm[2,]))
    return(salesratio);
}

salesratio1 = salesratio(lprice = lprice1,
                         display=display,
                         prddf = prddf, 
                         thetam = thetam)
salesratio2 = salesratio(lprice = lprice2,
                         display=display,
                         prddf = prddf, 
                         thetam = thetam)
salesratio3 = salesratio(lprice = lprice3,
                        display=display,
                        prddf = prddf, 
                        thetam = thetam)
salesratio4 = salesratio(lprice = lprice4,
                        display=display,
                        prddf = prddf, 
                        thetam = thetam)
salesratio5 = salesratio(lprice = lprice5,
                        display=display,
                        prddf = prddf, 
                        thetam = thetam)
list(c(salesratio1,salesratio2,salesratio3,salesratio4,salesratio5))

# we should set price at $0.74, which still keep salesratio above 2.
```
