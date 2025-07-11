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
test_cache_path = function(test_obj_file) {
  file.path(test_cache_dir(), test_obj_file)
}
test_cache_read = function(test_obj_file) {
  obj = test_cache_path(test_obj_file) |> readRDS()
  if (inherits(obj, "TMBModelSpec")) obj = mp_version_update(obj)
  return(obj)
}
test_cache_write = function(obj, test_obj_file) {
  saveRDS(obj, test_cache_path(test_obj_file))
}
