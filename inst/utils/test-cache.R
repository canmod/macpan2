test_cache_dir = function() {
  tools::R_user_dir("macpan2") |> file.path("test_cache")
}
test_cache_wipe = function() {
  test_cache = test_cache_dir()
  if (!dir.exists(test_cache)) dir.create(test_cache, recursive = TRUE)
  list.files(test_cache, full.names = TRUE) |> file.remove()
  return(test_cache)
}
test_cache_list = function() {
  test_cache_dir() |> list.files()
}
test_cache_read = function(test_obj_file) {
  file.path(test_cache_dir(), test_obj_file) |> readRDS()
}
test_cache_write = function(obj, test_obj_file) {
  saveRDS(obj, file.path(test_cache_dir(), test_obj_file))
}
