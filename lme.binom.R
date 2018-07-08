# tasoitus lmerillä

lme.binom.p1 <- function(df, F1, N, eps = 0) {
  # df dataframe jossa countit ja muu data
  # F1 muuttuja jossa jako 1 (esim. alueet)
  # N muuttuja jossa summat, esim. "N"
  
  Y <- select_(df, F1)
  
  alueet <- Y
  
  for (i in setdiff(names(df), c(F1, N))) {
    print(i)
    f <-paste0("cbind(`", i , "`," , N , "-`", i , "`) ~ (1|", F1, ") + 1")
    
    if (var(df[[i]]) > eps) {
      m <- do.call("glmer",
                   list(
                     as.formula(f),
                     data = as.name("df"),
                     family = as.name("binomial")
                   )
      )
      y <- data.frame(predict(m, alueet, type = "response"))
    } else {
      print(paste(i, "var is (almost) zero."))
      y <- data.frame(a = rep(NA, dim(alueet)[1]))
    }
    
    names(y) <- i
    
    y[[F1]] <- alueet[[F1]]
    Y <- merge(Y, y, by = F1)
  }
  return(Y)
}

lme.p2 <- function(df, F1, F2, eps = 0) {
  # df dataframe jossa muuttuja ja jaot
  # F1 muuttuja jossa jako 1 (esim. alueet hierarkia 1)
  # F2 muuttuja jossa jako 2 (esim. alueet hierarkia 2)
  
  Y <- select_(df, F1, F2)
  
  Y <- unique(Y)
  alueet <- Y
  
  for (i in setdiff(names(df), c(F1, F2))) {
    print(i)
    
    f <- paste0(i, "~ (1|", F1, ") + (1|", F2, ")  + 1")
    
    if (var(df[[i]], na.rm = TRUE) > eps) {
      m <- do.call("lmer", list(as.formula(f), data = as.name("df")))
      y <- data.frame(predict(m, alueet, type = "response"))
    } else {
      print(paste(i, "var is (almost) zero."))
      y <- data.frame(a = rep(NA, dim(alueet)[1]))
    }
    
    names(y) <- i
    y[[F1]] <- alueet[[F1]]
    y[[F2]] <- alueet[[F2]]
    Y <- merge(Y, y, by = c(F1, F2))
    
  }
  return(Y)
}

lme.binom.p2 <- function(df, F1, F2, N, eps = 0) {
  # df dataframe jossa countit ja muu data
  # F1 muuttuja jossa jako 1 (esim. alueet hierarkia 1)
  # F2 muuttuja jossa jako 2 (esim. alueet hierarkia 2)
  # N muuttuja jossa summat (N) esim. "N"
  
  Y <- select_(df, F1, F2)
  
  alueet <- Y
  
  for (i in setdiff(names(df), c(F1, F2, N))) {
    print(i)
    f <-
      paste0("cbind(`", i , "`," , N , "-`", i , "`) ~ (1|", F1, ") + (1|", F2, ")  + 1")
    
    if (var(df[[i]]) > eps) {
      m <-
        do.call("glmer",
                list(
                  as.formula(f),
                  data = as.name("df"),
                  family = as.name("binomial")
                ))
      y <- data.frame(predict(m, alueet, type = "response"))
    } else {
      print(paste(i, "var is (almost) zero."))
      y <- data.frame(a = rep(NA, dim(alueet)[1]))
    }
    
    names(y) <- i
    y[[F1]] <- alueet[[F1]]
    y[[F2]] <- alueet[[F2]]
    Y <- merge(Y, y, by = c(F1, F2))
    
  }
  return(Y)
}

lme.binom.p2.xcov <- function(df, F1, F2, N, x, eps = 0) {
  # df dataframe jossa countit ja muu data
  # F1 (str) df:n sarakkeen nimi, muuttuja jossa jako 1 (esim. alueet hierarkia 1)
  # F2 (str) df:n sarakkeen nimi, muuttuja, jossa jako 2 (esim. alueet hierarkia 2)
  # N (str) df:n sarakkeen nimi, muuttuja jossa summat (N) esim. "N"
  # x (str) df:n sarakkeen nimi jossa kovariaatti 1
  
  Y <- select_(df, F1, F2, x)
  
  alueet.cov <- Y
  
  for (i in setdiff(names(df), c(F1, F2, N, x))) {
    print(i)
    f <-
      paste0(
        "cbind(`",
        i ,
        "`," ,
        N ,
        "-`",
        i ,
        "`) ~ (1|",
        F1,
        ") + (1|",
        F2,
        ")  + (`",
        x,
        "`|",
        F1,
        ") + (`",
        x,
        "`|",
        F2,
        ")  + 1"
      )
    
    if (var(df[[i]]) > eps) {
      m <-
        do.call("glmer",
                list(
                  as.formula(f),
                  data = as.name("df"),
                  family = as.name("binomial")
                ))
      y <-
        data.frame(predict(m, alueet.cov, type = "response"))
    } else {
      print(paste(i, "var is (almost) zero."))
      y <- data.frame(a = rep(NA, dim(alueet)[1]))
    }
    
    names(y) <- i
    y[[F1]] <- alueet[[F1]]
    y[[F2]] <- alueet[[F2]]
    Y <- merge(Y, y, by = c(F1, F2))
    
  }
  return(Y)
}

# (N,i) ~ (x|F1) + (x|F2) + (1|F1) + (1|F2) + x + 1
# (N,i) ~ (x+1|F1) + (x+1|F2) + x + 1


lme.binom.p3 <- function(df, F1, F2, F3, N, eps = 0) {
  # df dataframe jossa countit ja muu data
  # F1 muuttuja jossa jako 1 (esim. alueet hierarkia 1)
  # F2 muuttuja jossa jako 2 (esim. alueet hierarkia 2)
  # F3 muuttuja jossa jako 2 (esim. alueet hierarkia 3)
  # N muuttuja jossa summat (N) esim "N"
  
  Y <- select_(df, F1, F2, F3)
  
  alueet <- Y
  
  for (i in setdiff(names(df), c(F1, F2, F3, N))) {
    print(i)
    f <-
      paste0("cbind(`",
             i ,
             "`," ,
             N ,
             "-`",
             i ,
             "`) ~ (1|",
             F1,
             ") + (1|",
             F2,
             ")  + (1|",
             F3,
             ")  + 1")
    
    if (var(df[[i]]) > eps) {
      m <-
        do.call("glmer",
                list(
                  as.formula(f),
                  data = as.name("df"),
                  family = as.name("binomial")
                ))
      y <- data.frame(predict(m, alueet, type = "response"))
    } else {
      print(paste(i, "var is (almost) zero."))
      y <- data.frame(a = rep(NA, dim(alueet)[1]))
    }
    
    names(y) <- i
    y[[F1]] <- alueet[[F1]]
    y[[F2]] <- alueet[[F2]]
    y[[F3]] <- alueet[[F3]]
    Y <- merge(Y, y, by = c(F1, F2, F3))
    
  }
  return(Y)
}