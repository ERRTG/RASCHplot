# Polytomous (partial credit) machinery: simulated data only.

sim_pcm <- function(N, tau, seed) {
  # tau: items x thresholds matrix (NA-padded for items with fewer categories)
  set.seed(seed)
  K <- nrow(tau)
  theta <- rnorm(N)
  dat <- sapply(1:K, function(i) {
    m <- sum(!is.na(tau[i, ]))
    cs <- cumsum(tau[i, 1:m])
    lg <- sapply(0:m, function(x) x * theta - if (x == 0) 0 else cs[x])
    p <- exp(lg - apply(lg, 1, max))
    p <- p / rowSums(p)
    apply(p, 1, function(r) sample(0:m, 1, prob = r))
  })
  colnames(dat) <- paste0("I", 1:K)
  dat[!(rowSums(dat) %in% c(0, sum(apply(tau, 1, function(x) sum(!is.na(x)))))), , drop = FALSE]
}

tau_uniform <- cbind(seq(-1.5, 1.5, length.out = 8) - 0.6,
                     seq(-1.5, 1.5, length.out = 8) + 0.6)
tau_mixed <- cbind(c(-1.2, -0.5, 0, 0.4, -0.8, 0.9, -0.3, 0.2),
                   c(-0.2, 0.3, 0.8, 1.1, 0.4, NA, 0.6, -0.1),
                   c(0.5, 1.2, NA, 1.6, NA, NA, 1.0, NA))

test_that("CML thresholds equal eRm thresholds (uniform widths)", {
  dat <- sim_pcm(400, tau_uniform, seed = 1)
  fit <- RASCHfits(method.item = "CML", method.person = "WML", dat = dat)
  erm <- as.matrix(eRm::thresholds(eRm::PCM(dat, sum0 = TRUE))$threshtable[[1]][, -1])
  expect_equal(unname(t(fit$delta)), unname(erm), tolerance = 1e-8)
})

test_that("CML thresholds equal eRm thresholds with mixed item widths", {
  dat <- sim_pcm(600, tau_mixed, seed = 2)
  fit <- RASCHfits(method.item = "CML", method.person = "WML", dat = dat)
  erm <- as.matrix(eRm::thresholds(eRm::PCM(dat, sum0 = TRUE))$threshtable[[1]][, -1])
  expect_equal(unname(t(fit$delta)), unname(erm), tolerance = 1e-8)
  # NA exactly where an item lacks a category
  expect_identical(unname(is.na(t(fit$delta))), unname(is.na(erm)))
})

test_that("pcmfct reproduces reference PCM category probabilities", {
  tau <- tau_mixed
  theta <- seq(-3, 3, length.out = 25)
  for (i in 1:nrow(tau)) {
    p <- RASCHplot:::pcmfct(delta = tau, theta = theta, ii = i)
    m <- sum(!is.na(tau[i, ]))
    cs <- c(0, cumsum(tau[i, 1:m]))
    ref <- exp(sapply(0:m, function(x) x * theta - cs[x + 1]))
    ref <- ref / rowSums(ref)
    expect_equal(unname(p[, 1:(m + 1)]), unname(ref), tolerance = 1e-10)
  }
})

test_that("model-expected item means match observed item means (CML, mixed widths)", {
  dat <- sim_pcm(800, tau_mixed, seed = 3)
  fit <- RASCHfits(method.item = "CML", method.person = "WML", dat = dat)
  expected <- sapply(1:ncol(dat), function(i) {
    m <- sum(!is.na(tau_mixed[i, ]))
    cs <- c(0, cumsum(fit$delta[1:m, i]))
    lg <- sapply(0:m, function(x) x * fit$theta - cs[x + 1])
    p <- exp(lg - apply(lg, 1, max))
    p <- p / rowSums(p)
    mean(p %*% (0:m))
  })
  expect_gt(cor(colMeans(dat), expected), 0.999)
})

test_that("MML thresholds equal TAM's xsi (uniform widths)", {
  dat <- sim_pcm(300, tau_uniform, seed = 9)
  fit <- RASCHfits(method.item = "MML", method.person = "WML", dat = dat)
  mi <- apply(dat, 2, max)
  delta_vec <- unlist(lapply(seq_along(mi), function(i) fit$delta[1:mi[i], i]))
  expect_equal(unname(delta_vec), unname(fit$item.fit$xsi$xsi), tolerance = 1e-8)
})

test_that("RMPstats: item order invariance", {
  dat <- sim_pcm(500, tau_uniform, seed = 4)
  fit <- RASCHfits(method.item = "CML", method.person = "WML", dat = dat)
  s1 <- RMPstats(delta = t(fit$delta), theta = fit$theta, dat = dat)
  perm <- c(8:1)
  fit2 <- RASCHfits(method.item = "CML", method.person = "WML", dat = dat[, perm])
  s2 <- RMPstats(delta = t(fit2$delta), theta = fit2$theta, dat = dat[, perm])
  expect_equal(unname(s2$Outfit), unname(s1$Outfit[perm]), tolerance = 1e-4)
  expect_equal(unname(s2$Infit), unname(s1$Infit[perm]), tolerance = 1e-4)
})

test_that("observed fit statistics are near 1 for data from the model", {
  dat <- sim_pcm(500, tau_uniform, seed = 5)
  fit <- RASCHfits(method.item = "CML", method.person = "WML", dat = dat)
  s <- RMPstats(delta = t(fit$delta), theta = fit$theta, dat = dat)
  expect_true(all(s$Outfit > 0.6 & s$Outfit < 1.5))
  expect_true(all(s$Infit > 0.6 & s$Infit < 1.5))
})

test_that("RMPstats refuses undefined person estimates", {
  dat <- sim_pcm(200, tau_uniform, seed = 6)
  fit <- RASCHfits(method.item = "CML", method.person = "WML", dat = dat)
  th <- fit$theta
  th[1] <- NA
  expect_error(RMPstats(delta = t(fit$delta), theta = th, dat = dat), "NA")
})

test_that("rRMP produces data with the right shape and no constant items", {
  set.seed(7)
  X <- RASCHplot::rRMP(delta = tau_mixed, theta = rnorm(300), B = 2)
  expect_length(X, 2)
  for (b in 1:2) {
    expect_equal(ncol(X[[b]]), 8)
    expect_true(all(apply(X[[b]], 2, function(x) length(unique(x)) > 1)))
  }
})

test_that("rRMPstats runs end to end and records attempts", {
  dat <- sim_pcm(300, tau_uniform, seed = 8)
  fit <- RASCHfits(method.item = "CML", method.person = "WML", dat = dat)
  obj <- rRMPstats(delta = t(fit$delta), theta = fit$theta, method.item = "CML",
                   method.person = "WML", B = 5)
  expect_length(obj$statobj, 5)
  expect_length(obj$attempts, 5)
  expect_true(all(obj$attempts >= 1))
  expect_true(all(is.finite(sapply(obj$statobj, function(s) max(s$Outfit)))))
})

test_that("Q3 runs on polytomous data (thresholds x items delta is transposed correctly)", {
  dat <- sim_pcm(400, tau_uniform, seed = 10)
  q <- Q3(items = dat, method.item = "CML", method.person = "WML", model = "RMP", standardize = FALSE)
  expect_true(is.finite(q$Q3max))
  expect_equal(dim(q$Q3matrix), c(8L, 8L))
})
