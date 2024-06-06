test_that("matrices used in functions that need matrix simulation history are automatically saved at model specification step", {
  
  set.seed(1L)
  during = list(A ~ alpha * rpois(time_step(0)), B ~ convolution(A,kernel))
  after = list(C ~ rbind_time(B,t))
  default = list(kernel = c(0.5,0.3,0.2), alpha = 0.2)
  integers = list(t = seq(0,10,2))
  
  spec_auto_save = mp_tmb_model_spec(during = during
     , after = after
     , default = default
     , integers = integers
     )
  
  spec_force_save = mp_tmb_model_spec(during = during
     , after = after
     , default = default
     , integers = integers
     , must_save = c("A","B")
  )
  
  simB_auto_save = mp_simulator(spec_auto_save, 12L, "B") 
  simB_force_save =  mp_simulator(spec_force_save, 12L, "B")
  
  simC_auto_save = mp_simulator(spec_auto_save, 12L, "C") 
  simC_force_save =  mp_simulator(spec_force_save, 12L, "C")
  
  expect_equal(mp_trajectory(simB_auto_save),mp_trajectory(simB_force_save))
  expect_equal(mp_final(simC_auto_save),mp_final(simC_force_save))
  # is this redundant?
  expect_equal(mp_final(simB_auto_save),mp_final(simB_force_save))
})
