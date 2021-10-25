#' @import basilisk
env_sbpack <- basilisk::BasiliskEnvironment("env_sbpack", pkgname="basilisk",
                                            packages = "python=3.8",
                                            pip = c("sbpack==2020.10.5"))
