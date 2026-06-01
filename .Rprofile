# Auto-set library path if running on the server (where /home/user/R/library exists)
server_lib <- "/home/user/R/library"
if (dir.exists(server_lib) && !(server_lib %in% .libPaths())) {
  .libPaths(c(server_lib, .libPaths()))
}
