#' Calibrating risk-neutral parameters
#'
#' @description
#' Function that returns the calibrated risk-neutral parameters and in-sample pricing errors based on the market prices
#'
#' @param theta A q times 1 vector
#' @param price A k times 1 vector containing the computed futures prices
#' @param market_price A k times 1 vector containing the actual futures prices traded in the market
#'
#' @return A list containing:
#'         \item{}{}
#'         \item{}{}
#'         \item{}{}
#'
#' @export
#'
#' @examples
calib = function(theta, price, market_price) {
  # compatibility checks
  if (is.vector(theta) == FALSE) {
    stop("theta should be a vector.") # returns error message if the input theta is not a vector
  }
  if (is.vector(price) == FALSE) {
    stop("price should be a vector.") # returns error message if the input price is not a vector
  }
  if (is.vector(market_price) == FALSE) {
    stop("market_price should be a vector.") # returns error message if the input market_price is not a vector
  }

  # computing price with risk-neutral parameter
  alpha1 = read_excel("alpha1.xlsx")
  alpha1 = as.matrix(alpha1[,2:226])
  alpha1 = unname(alpha1)
  alpha2 = read_excel("alpha2.xlsx")
  alpha2 = as.matrix(alpha2[,2:226])
  alpha2 = unname(alpha2)
  alpha3 = read_excel("alpha3.xlsx")
  alpha3 = as.matrix(alpha3[,2:226])
  alpha3 = unname(alpha3)
  ealpha1 = read_excel("ealpha1.xlsx")
  ealpha1 = as.matrix(ealpha1[,2:226])
  ealpha1 = unname(ealpha1)
  ealpha2 = read_excel("ealpha2.xlsx")
  ealpha2 = as.matrix(ealpha2[,2:226])
  ealpha2 = unname(ealpha2)
  ealpha3 = read_excel("ealpha3.xlsx")
  ealpha3 = as.matrix(ealpha3[,2:226])
  ealpha3 = unname(ealpha3)
  residuals = read_excel("allresiduals.xlsx")
  residuals = as.matrix(residuals[,3:227])
  residuals = unname(residuals)
  stations = read_excel("tempstations.xlsx")
  s1 = stations$longitude
  s2 = stations$latitude
  X1 = s1[1:13]
  X2 = s1[14:225]
  Y1 = s2[1:13]
  Y2 = s2[14:225]
  coords = cbind(s1, s2)
  xtnig = read_excel("xtnig.xlsx")
  xtnig = as.matrix(xtnig[,3:227])
  xtnig = unname(xtnig)
  xtgal = read_excel("xtgal.xlsx")
  xtgal = as.matrix(xtgal[,3:227])
  xtgal = unname(xtgal)
  sigma2eps = read_excel("sigma2eps.xlsx")
  sigma2eps = as.matrix(sigma2eps[,3])
  sigma2eps = unname(sigma2eps)
  GH = read_excel("GH.xlsx")
  GHmu = GH$mu
  GHdelta = GH$delta
  GHalpha = GH$alpha
  GHbeta = GH$beta
  GHlambda = GH$lambda
  NIG = read_excel("NIG.xlsx")
  NIGmu = NIG$mu
  NIGdelta = NIG$delta
  NIGalpha = NIG$alpha
  NIGbeta = NIG$beta
  VG = read_excel("VG.xlsx")
  VGmu = VG$mu
  VGalpha = VG$alpha
  VGbeta = VG$beta
  VGlambda = VG$lambda
  H = read_excel("H.xlsx")
  Hmu = H$mu
  Hdelta = H$delta
  Halpha = H$alpha
  Hbeta = H$beta
  t = read_excel("T.xlsx")
  Tmu = t$mu
  Tdelta = t$delta
  Tbeta = t$beta
  Tlambda = t$lambda
  sNIG = read_excel("sNIG.xlsx")
  sNIGmu = sNIG$mu
  sNIGdelta = sNIG$delta
  sNIGalpha = sNIG$alpha
  sNIGbeta = sNIG$beta
  sVG = read_excel("sVG.xlsx")
  sVGmu = sVG$mu
  sVGalpha = sVG$alpha
  sVGbeta = sVG$beta
  sVGlambda = sVG$lambda
  seasonal_coefs = read_excel("seasonal_coefs.xlsx")
  seacoefa = seasonal_coefs$a
  seacoefb = seasonal_coefs$b
  seacoefc = seasonal_coefs$c
  seacoefd = seasonal_coefs$d
  smt1 = read_excel("y0sol1.xlsx")
  smt1 = as.matrix(smt1)
  smt1 = unname(smt1)
  smt2 = read_excel("y0sol2.xlsx")
  smt2 = as.matrix(smt2)
  smt2 = unname(smt2)
  smt3 = read_excel("y0sol3.xlsx")
  smt3 = as.matrix(smt3)
  smt3 = unname(smt3)
  smt4 = read_excel("y0sol4.xlsx")
  smt4 = as.matrix(smt4)
  smt4 = unname(smt4)
  smt = cbind(smt1, smt2, smt3, smt4)
  options(digits=22)

  riemann = function(f, left, right, step){
    theta = seq(from = left,to = right,by = step)
    fall = c()
    for (i in 1:length(theta)) {
      fall[i] = f(theta[i])
    }
    sum = mean(fall)*(right-left)
    return(sum)
  }

  seasonal = function(t, a, b, c, d) {
    return(a + b*t + c*cos(2*pi*(t-d)/365))
  }

  c = 65
  i = complex(real=0,imag=1)
  charGH = function(u, mu, delta, alpha, beta, lambda) {
    return(exp(i*u*mu)*((alpha^2-beta^2)/(alpha^2-(beta+i*u)^2))^(lambda/2)*(BesselK(abs(lambda),Re(delta*sqrt(alpha^2-(beta+i*u)^2)))/BesselK(abs(lambda),delta*sqrt(alpha^2-beta^2))))
  }
  charNIG = function(u, mu, delta, alpha, beta) {
    return(exp((i*u*mu)+delta*(sqrt(alpha^2-beta^2)-sqrt(alpha^2-(beta+i*u)^2))))
  }
  charVG = function(u, mu, alpha, beta, lambda) {
    return(exp(i*u*mu)*((alpha^2-beta^2)/(alpha^2-(beta+i*u)^2))^lambda)
  }
  charH = function(u, mu, delta, alpha, beta) {
    return(exp(i*u*mu)*(sqrt(alpha^2-beta^2)/sqrt(alpha^2-(beta+i*u)^2))*(BesselK(1,Re(delta*sqrt(alpha^2-(beta+i*u)^2)))/BesselK(1,delta*sqrt(alpha^2-beta^2))))
  }
  charT = function(u, mu, delta, beta, lambda) {
    return(exp(i*u*mu)*(2/delta)^lambda*(2*BesselK(abs(lambda),delta*abs(u))/(gamma(abs(lambda))*abs(u)^lambda)))
  }
  meanNIG = function(mu, delta, alpha, beta) {
    return(mu + (delta*beta)/sqrt(alpha^2-beta^2))
  }
  meanGAL = function(mu, alpha, beta, lambda) {
    return(mu + (2*lambda*beta)/(alpha^2-beta^2))
  }

  Xinig = function(tt) {
    mat = c()
    if (tt == 1 | tt == 2 | tt == 3) {
      for (r in 1:66) {
        mat = rbind(mat, xtnig[365*r+tt-3,])
      }
    } else if (tt > 3 & tt <= 365) {
      for (r in 1:67) {
        mat = rbind(mat, xtnig[365*(r-1)+tt-3,])
      }
    }
    return(cov(mat))
  }
  Xigal = function(tt) {
    mat = c()
    if (tt == 1 | tt == 2 | tt == 3) {
      for (r in 1:66) {
        mat = rbind(mat, xtgal[365*r+tt-3,])
      }
    } else if (tt > 3 & tt <= 365) {
      for (r in 1:67) {
        mat = rbind(mat, xtgal[365*(r-1)+tt-3,])
      }
    }
    return(cov(mat))
  }
  damp = 0.01

  k = 4
  tau1 = 25002
  tau2 = 25032

  ttt = c(24998, 25000, 25001)
  prices = c(330, 320, 310)
  mMPR = c()
  mMPVR = c()

  for (pp in 1:length(ttt)) {
    t = ttt[pp]
    price = prices[pp]

    tu1 = 1
    tu2 = 1
    outputt = c()
    for (j in tau1:tau2) {
      mcoef = ealpha3-alpha3%*%ealpha3
      mcoef1 = -ealpha1 - alpha1%*%ealpha1
      mcoef2 = -alpha2%*%ealpha2
      myode = function(t,state,parms) {
        with(as.list(state),{
          dndt = rep(0, length(state))

          for (p in 1:225) {
            dndt[p] = sum(mcoef[p,]%*%yx[1:225]) + sum(mcoef1[p,]%*%yx[226:450]) + sum(mcoef2[p,]%*%yx[451:675])
          }

          return(list(dndt))
        })
      }
      yx = c(residuals[t,], residuals[t-1,], residuals[t-2,])
      # tts = seq(t, j+2, 1)
      tts = seq(t, j, 1)
      out = ode(y=yx, time=tts, func = myode, parms = NULL)
      output = unlist(out[nrow(out),])
      output = unname(output)
      output1 = output[2:226]
      output1[k] = output1[k]*0.001
      # output1 = output[227:451]
      # output1 = output[452:676]
      outputt = cbind(outputt, output1)
    }
    outputt = unname(outputt)

    sol1 = c()
    ans = matrix(0, (tau2-tau1+1), (tau2-t+1))
    ans2 = matrix(0, (tau2-tau1+1), (tau2-t+1))
    for (j in tau1:tau2) {
      l = j
      outputl = outputt[,(l-tau1+1)]
      sol1[j-tau1+1] = seasonal(j,seacoefa[k],seacoefb[k],seacoefc[k],seacoefd[k]) + outputl[k] - c
      plus = 0
      for (q in t:l) {
        sm = smt[,((l-q)*225+1):((l-q)*225+225)]
        su = matrix(0, 225, 225)
        su2 = matrix(0, 225, 225)
        for (g in t:q) {
          if (g > 24820) {
            tttt = g - 24820
          } else {
            tttt = g - 24455
          }
          Xi = Xinig(tttt) + diag(sigma2eps[g], 225, 225)
          Xi2 = Xigal(tttt) + diag(sigma2eps[g], 225, 225)
          su = su + Xi
          su2 = su2 + Xi2
        }
        pro = sm%*%su
        pro2 = sm%*%su2
        ant = pro[k,]
        ant2 = pro2[k,]
        ans[(j-tau1+1),(q-t+1)] = sum(ant)
        ans2[(j-tau1+1),(q-t+1)] = sum(ant2)
      }
    }

    mu = GHmu[k]
    delta = GHdelta[k]
    alpha = GHalpha[k]
    lambda = GHlambda[k]
    muvol = sNIGmu[k]
    deltavol = sNIGdelta[k]
    alphavol = sNIGalpha[k]
    beta = c(GHbeta[k], sNIGbeta[k])
    FCDDGHnig = function(beta) {
      plus2 = 0
      for (j in tau1:tau2) {
        l = j
        plus = 0
        for (q in t:l) {
          pros = meanNIG(muvol, deltavol, alphavol, beta[2])*ans[(j-tau1+1),(q-t+1)]
          plus = plus + charGH(pros, mu, delta, alpha, beta[1], lambda)
        }
        result = sol1[j-tau1+1] + plus
        plus2 = plus2 + result
      }
      return(plus2)
    }
    # FCDDGHnig(beta)

    calib1 = function(beta) {
      return((tu1*Re(FCDDGHnig(beta))-price)^2)
    }

    old <- Sys.time()
    betas1 = optim(c(GHbeta[k], sNIGbeta[k]), calib1)
    new <- Sys.time() - old
    print(new)

    MPR1 = betas1$par[1] - GHbeta[k]
    MPVR1 = betas1$par[2] - sNIGbeta[k]

    mu = GHmu[k]
    delta = GHdelta[k]
    alpha = GHalpha[k]
    lambda = GHlambda[k]
    muvol = sVGmu[k]
    lambdavol = sVGlambda[k]
    alphavol = sVGalpha[k]
    beta = c(GHbeta[k], sVGbeta[k])
    FCDDGHgal = function(beta) {
      plus2 = 0
      for (j in tau1:tau2) {
        l = j
        plus = 0
        for (q in t:l) {
          pros = meanGAL(muvol, alphavol, beta[2], lambdavol)*ans2[(j-tau1+1),(q-t+1)]
          plus = plus + charGH(pros*damp, mu, delta, alpha, beta[1], lambda)
        }
        result = sol1[j-tau1+1] + plus
        plus2 = plus2 + result
      }
      return(plus2)
    }
    # FCDDGHgal(beta)

    calib2 = function(beta) {
      return((tu2*Re(FCDDGHgal(beta))-price)^2)
    }

    old <- Sys.time()
    betas2 = optim(c(GHbeta[k], sVGbeta[k]), calib2)
    new <- Sys.time() - old
    print(new)

    MPR2 = betas2$par[1] - GHbeta[k]
    MPVR2 = betas2$par[2] - sVGbeta[k]

    mu = NIGmu[k]
    delta = NIGdelta[k]
    alpha = NIGalpha[k]
    muvol = sNIGmu[k]
    deltavol = sNIGdelta[k]
    alphavol = sNIGalpha[k]
    beta = c(NIGbeta[k], sNIGbeta[k])
    FCDDNIGnig = function(beta) {
      plus2 = 0
      for (j in tau1:tau2) {
        l = j
        plus = 0
        for (q in t:l) {
          pros = meanNIG(muvol, deltavol, alphavol, beta[2])*ans[(j-tau1+1),(q-t+1)]
          plus = plus + charNIG(pros, mu, delta, alpha, beta[1])
        }
        result = sol1[j-tau1+1] + plus
        plus2 = plus2 + result
      }
      return(plus2)
    }

    calib3 = function(beta) {
      return((tu1*Re(FCDDNIGnig(beta))-price)^2)
    }

    betas3 = optim(c(NIGbeta[k], sNIGbeta[k]), calib3)
    MPR3 = betas3$par[1] - NIGbeta[k]
    MPVR3 = betas3$par[2] - sNIGbeta[k]

    mu = NIGmu[k]
    delta = NIGdelta[k]
    alpha = NIGalpha[k]
    muvol = sVGmu[k]
    lambdavol = sVGlambda[k]
    alphavol = sVGalpha[k]
    beta = c(NIGbeta[k], sVGbeta[k])
    FCDDNIGgal = function(beta) {
      plus2 = 0
      for (j in tau1:tau2) {
        l = j
        plus = 0
        for (q in t:l) {
          pros = meanGAL(muvol, alphavol, beta[2], lambdavol)*ans2[(j-tau1+1),(q-t+1)]
          plus = plus + charNIG(pros*damp, mu, delta, alpha, beta[1])
        }
        result = sol1[j-tau1+1] + plus
        plus2 = plus2 + result
      }
      return(plus2)
    }

    calib4 = function(beta) {
      return((tu2*Re(FCDDNIGgal(beta))-price)^2)
    }

    betas4 = optim(c(NIGbeta[k], sVGbeta[k]), calib4)
    MPR4 = betas4$par[1] - NIGbeta[k]
    MPVR4 = betas4$par[2] - sVGbeta[k]

    mu = VGmu[k]
    lambda = VGlambda[k]
    alpha = VGalpha[k]
    muvol = sNIGmu[k]
    deltavol = sNIGdelta[k]
    alphavol = sNIGalpha[k]
    beta = c(VGbeta[k], sNIGbeta[k])
    FCDDVGnig = function(beta) {
      plus2 = 0
      for (j in tau1:tau2) {
        l = j
        plus = 0
        for (q in t:l) {
          pros = meanNIG(muvol, deltavol, alphavol, beta[2])*ans[(j-tau1+1),(q-t+1)]
          plus = plus + charVG(pros, mu, alpha, beta[1], lambda)
        }
        result = sol1[j-tau1+1] + plus
        plus2 = plus2 + result
      }
      return(plus2)
    }

    calib5 = function(beta) {
      return((tu1*Re(FCDDVGnig(beta))-price)^2)
    }

    betas5 = optim(c(VGbeta[k], sNIGbeta[k]), calib5)
    MPR5 = betas5$par[1] - VGbeta[k]
    MPVR5 = betas5$par[2] - sNIGbeta[k]

    mu = VGmu[k]
    lambda = VGlambda[k]
    alpha = VGalpha[k]
    muvol = sVGmu[k]
    lambdavol = sVGlambda[k]
    alphavol = sVGalpha[k]
    beta = c(VGbeta[k], sVGbeta[k])
    FCDDVGgal = function(beta) {
      plus2 = 0
      for (j in tau1:tau2) {
        l = j
        plus = 0
        for (q in t:l) {
          pros = meanGAL(muvol, alphavol, beta[2], lambdavol)*ans2[(j-tau1+1),(q-t+1)]
          plus = plus + charVG(pros*damp, mu, alpha, beta[1], lambda)
        }
        result = sol1[j-tau1+1] + plus
        plus2 = plus2 + result
      }
      return(plus2)
    }

    calib6 = function(beta) {
      return((tu2*Re(FCDDVGgal(beta))-price)^2)
    }

    betas6 = optim(c(VGbeta[k], sVGbeta[k]), calib6)
    MPR6 = betas6$par[1] - VGbeta[k]
    MPVR6 = betas6$par[2] - sVGbeta[k]

    mu = Hmu[k]
    delta = Hdelta[k]
    alpha = Halpha[k]
    muvol = sNIGmu[k]
    deltavol = sNIGdelta[k]
    alphavol = sNIGalpha[k]
    beta = c(Hbeta[k], sNIGbeta[k])
    FCDDHnig = function(beta) {
      plus2 = 0
      for (j in tau1:tau2) {
        l = j
        plus = 0
        for (q in t:l) {
          pros = meanNIG(muvol, deltavol, alphavol, beta[2])*ans[(j-tau1+1),(q-t+1)]
          plus = plus + charH(pros, mu, delta, alpha, beta[1])
        }
        result = sol1[j-tau1+1] + plus
        plus2 = plus2 + result
      }
      return(plus2)
    }

    calib7 = function(beta) {
      return((tu1*Re(FCDDHnig(beta))-price)^2)
    }

    betas7 = optim(c(Hbeta[k], sNIGbeta[k]), calib7)
    MPR7 = betas7$par[1] - Hbeta[k]
    MPVR7 = betas7$par[2] - sNIGbeta[k]

    mu = Hmu[k]
    delta = Hdelta[k]
    alpha = Halpha[k]
    muvol = sVGmu[k]
    lambdavol = sVGlambda[k]
    alphavol = sVGalpha[k]
    beta = c(Hbeta[k], sVGbeta[k])
    FCDDHgal = function(beta) {
      plus2 = 0
      for (j in tau1:tau2) {
        l = j
        plus = 0
        for (q in t:l) {
          pros = meanGAL(muvol, alphavol, beta[2], lambdavol)*ans2[(j-tau1+1),(q-t+1)]
          plus = plus + charH(pros*damp, mu, delta, alpha, beta[1])
        }
        result = sol1[j-tau1+1] + plus
        plus2 = plus2 + result
      }
      return(plus2)
    }

    calib8 = function(beta) {
      return((tu2*Re(FCDDHgal(beta))-price)^2)
    }

    betas8 = optim(c(Hbeta[k], sVGbeta[k]), calib8)
    MPR8 = betas8$par[1] - Hbeta[k]
    MPVR8 = betas8$par[2] - sVGbeta[k]

    mu = Tmu[k]
    delta = Tdelta[k]
    lambda = Tlambda[k]
    muvol = sNIGmu[k]
    deltavol = sNIGdelta[k]
    alphavol = sNIGalpha[k]
    beta = c(Tbeta[k], sNIGbeta[k])
    FCDDTnig = function(beta) {
      plus2 = 0
      for (j in tau1:tau2) {
        l = j
        plus = 0
        for (q in t:l) {
          pros = meanNIG(muvol, deltavol, alphavol, beta[2])*ans[(j-tau1+1),(q-t+1)]
          plus = plus + charT(pros, mu, delta, beta[1], lambda)
        }
        result = sol1[j-tau1+1] + plus
        plus2 = plus2 + result
      }
      return(plus2)
    }

    calib9 = function(beta) {
      return((tu1*Re(FCDDTnig(beta))-price)^2)
    }

    betas9 = optim(c(Tbeta[k], sNIGbeta[k]), calib9)
    MPR9 = betas9$par[1] - Tbeta[k]
    MPVR9 = betas9$par[2] - sNIGbeta[k]

    mu = Tmu[k]
    delta = Tdelta[k]
    lambda = Tlambda[k]
    muvol = sVGmu[k]
    lambdavol = sVGlambda[k]
    alphavol = sVGalpha[k]
    beta = c(Tbeta[k], sVGbeta[k])
    FCDDTgal = function(beta) {
      plus2 = 0
      for (j in tau1:tau2) {
        l = j
        plus = 0
        for (q in t:l) {
          pros = meanGAL(muvol, alphavol, beta[2], lambdavol)*ans2[(j-tau1+1),(q-t+1)]
          plus = plus + charT(pros*damp, mu, delta, beta[1], lambda)
        }
        result = sol1[j-tau1+1] + plus
        plus2 = plus2 + result
      }
      return(plus2)
    }

    calib10 = function(beta) {
      return((tu2*Re(FCDDTgal(beta))-price)^2)
    }

    betas10 = optim(c(Tbeta[k], sVGbeta[k]), calib10)
    MPR10 = betas10$par[1] - Tbeta[k]
    MPVR10 = betas10$par[2] - sVGbeta[k]

    MPRs = c(MPR1, MPR2, MPR3, MPR4, MPR5, MPR6, MPR7, MPR8, MPR9, MPR10)
    MPVRs = c(MPVR1, MPVR2, MPVR3, MPVR4, MPVR5, MPVR6, MPVR7, MPVR8, MPVR9, MPVR10)
    mMPR = rbind(mMPR, MPRs)
    mMPVR = rbind(mMPVR, MPVRs)
  }

  MPRsd = data.frame(mMPR)
  write.xlsx(MPRsd, "MPRs2.xlsx")

  MPVRsd = data.frame(mMPVR)
  write.xlsx(MPVRsd, "MPVRs2.xlsx")


  MPRs = c(MPR1, MPR2, MPR3, MPR4, MPR5, MPR6, MPR7, MPR8, MPR9, MPR10)
  MPRsd = data.frame(t(MPRs))
  write.xlsx(MPRsd, "MPRs.xlsx")

  MPVRs = c(MPVR1, MPVR2, MPVR3, MPVR4, MPVR5, MPVR6, MPVR7, MPVR8, MPVR9, MPVR10)
  MPVRsd = data.frame(t(MPVRs))
  write.xlsx(MPVRsd, "MPVRs.xlsx")

  # returns the objective function for minimisation
  return(theta^2 - 2 * theta * market_price - market_price^2)
}
