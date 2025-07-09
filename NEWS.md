## Changes in 2.6.1

In-Progress

[source](https://github.com/canmod/macpan2)
 | [change list](https://github.com/canmod/macpan2/compare/74e55c668cbb406c99ecb7dd984a2d0b3b1ac2c9..ccd3ef3259302dfb1171f202aa0cdfcf7b569492)

## Changes in 2.6.0

Released [2025-07-07]

[source](https://github.com/canmod/macpan2/tree/74e55c668cbb406c99ecb7dd984a2d0b3b1ac2c9)
 | [change list](https://github.com/canmod/macpan2/compare/64a2012dd8a22396f2f823fc8b2927960c282231..74e55c668cbb406c99ecb7dd984a2d0b3b1ac2c9)

### New Features

* Engine function `cumsum`.
* Warning message when checking for `macpan2` version mismatch when `macpan2` is loaded but not installed.


## Changes in 2.5.0

Released [2025-06-27]

[source](https://github.com/canmod/macpan2/tree/64a2012dd8a22396f2f823fc8b2927960c282231)
 | [change list](https://github.com/canmod/macpan2/compare/f39d85eeefe539c9d971b9d28bd028411bb9957e..64a2012dd8a22396f2f823fc8b2927960c282231)

### New Features

* [Code of conduct](* [Code of conduct](https://github.com/canmod/macpan2?tab=coc-ov-file#readme).


## Changes in 2.4.1

Released [2025-06-21]

[source](https://github.com/canmod/macpan2/tree/f39d85eeefe539c9d971b9d28bd028411bb9957e)
 | [change list](https://github.com/canmod/macpan2/compare/4f3773d1cb91057afae0a867cecfe7efe148abe2..f39d85eeefe539c9d971b9d28bd028411bb9957e)

### Bug Fixes

* [#326](https://github.com/canmod/macpan2/issues/326)


## Changes in 2.4.0

Released [2025-06-16]

[source](https://github.com/canmod/macpan2/tree/4f3773d1cb91057afae0a867cecfe7efe148abe2)
 | [change list](https://github.com/canmod/macpan2/compare/3b2e2c8e184556851f6ea9c7dabab5e21f42b476..4f3773d1cb91057afae0a867cecfe7efe148abe2)

### Bug Fixes

* [#263](https://github.com/canmod/macpan2/issues/263)
* [#332](https://github.com/canmod/macpan2/issues/332)
* [#333](https://github.com/canmod/macpan2/issues/333)


## Changes in 2.3.4

Released [2025-06-12]

[source](https://github.com/canmod/macpan2/tree/3b2e2c8e184556851f6ea9c7dabab5e21f42b476)
 | [change list](https://github.com/canmod/macpan2/compare/fe2f65a7f73452c90b12a7fc4530bc40afa05517..3b2e2c8e184556851f6ea9c7dabab5e21f42b476)

### Bug Fixes

* Default clamping parameters now ensure that the `clamp` engine function is twice-differentiable.
* Minor enhancements to the docs and handling of [mp_version_update](https://canmod.github.io/macpan2/reference/mp_version_update.html) and [mp_read_rds](https://canmod.github.io/macpan2/reference/mp_read_rds.html).


## Changes in 2.3.3

Released [2025-06-04]

[source](https://github.com/canmod/macpan2/tree/fe2f65a7f73452c90b12a7fc4530bc40afa05517)
 | [change list](https://github.com/canmod/macpan2/compare/ddbacc1da5d5261f5763571ec527d93e326c7494..fe2f65a7f73452c90b12a7fc4530bc40afa05517)

### Bug Fixes

* Fix regression bug causing `mp_absolute_flow` to fail.


## Changes in 2.3.2

Released [2025-06-03]

[source](https://github.com/canmod/macpan2/tree/ddbacc1da5d5261f5763571ec527d93e326c7494)
 | [change list](https://github.com/canmod/macpan2/compare/d14d76e0642063dfa93149b9165cf6e4eebb0848..ddbacc1da5d5261f5763571ec527d93e326c7494)

### Bug Fixes

* Fix bug in absolute flows and test.


## Changes in 2.3.1

Released [2025-06-02]

[source](https://github.com/canmod/macpan2/tree/d14d76e0642063dfa93149b9165cf6e4eebb0848)
 | [change list](https://github.com/canmod/macpan2/compare/2b448729a223b1b4bd3d5937215019cbf4618bb2..d14d76e0642063dfa93149b9165cf6e4eebb0848)

## Changes in 2.3.0

Released [2025-06-02]

[source](https://github.com/canmod/macpan2/tree/2b448729a223b1b4bd3d5937215019cbf4618bb2)
 | [change list](https://github.com/canmod/macpan2/compare/a25df4d0785c3ccf760350895889e4a4ba0f93c1..2b448729a223b1b4bd3d5937215019cbf4618bb2)

### New Features

* Absolute flows in model specifications are no longer experimental.


## Changes in 2.2.2

Released [2025-06-02]

[source](https://github.com/canmod/macpan2/tree/a25df4d0785c3ccf760350895889e4a4ba0f93c1)
 | [change list](https://github.com/canmod/macpan2/compare/75f23ab2e8716d45ae4e343771b3a6196216fd54..a25df4d0785c3ccf760350895889e4a4ba0f93c1)

This version patched `2.2.1`, which was released in a broken state.

### New Features

* [mp_version_update](https://canmod.github.io/macpan2/reference/mp_version_update.html)
* [mp_read_rds](https://canmod.github.io/macpan2/reference/mp_read_rds.html)

### Build Tools

* Avoid source code compilation during `roxygen` steps, because these steps use low optimization level and therefore results in slower simulations.


## Changes in 2.2.0

Released [2025-05-27]

[source](https://github.com/canmod/macpan2/tree/75f23ab2e8716d45ae4e343771b3a6196216fd54)
 | [change list](https://github.com/canmod/macpan2/compare/d6b0e9901e39fbc22d1b1dd4ec24b6ad542cebb8..75f23ab2e8716d45ae4e343771b3a6196216fd54)

## Changes in 2.1.0

Released [2025-05-23]

[source](https://github.com/canmod/macpan2/tree/d6b0e9901e39fbc22d1b1dd4ec24b6ad542cebb8)
 | [change list](https://github.com/canmod/macpan2/compare/c48fb449c8530240fba3bdddfa44af22cb50529d..d6b0e9901e39fbc22d1b1dd4ec24b6ad542cebb8)

### New Features

* [mp_version](https://canmod.github.io/macpan2/reference/mp_version.html)
* [mp_inflow](https://canmod.github.io/macpan2/reference/mp_inflow.html) (experimental)
* [mp_outflow](https://canmod.github.io/macpan2/reference/mp_outflow.html) (experimental)

### Build Tools

* Refresh `NEWS.md` generation tools.


## Changes in 2.0.1

Released [2025-05-21]

[source](https://github.com/canmod/macpan2/tree/c48fb449c8530240fba3bdddfa44af22cb50529d)
 | [change list](https://github.com/canmod/macpan2/compare/a372c7a909de89c8e87f2b0ac5935ccb4a198bb3..c48fb449c8530240fba3bdddfa44af22cb50529d)

### New Features

* [mp_log1p](https://canmod.github.io/macpan2/reference/mp_log1p.html)


## Changes in 2.0.0

Released [2025-05-16]

[source](https://github.com/canmod/macpan2/tree/a372c7a909de89c8e87f2b0ac5935ccb4a198bb3)
 | [change list](https://github.com/canmod/macpan2/compare/a99e1b9b6759ac46214c3ecc2bee694113aab88a..a372c7a909de89c8e87f2b0ac5935ccb4a198bb3)

### Behaviour Changes

* [#93](https://github.com/canmod/macpan2/issues/93)
* [#309](https://github.com/canmod/macpan2/issues/309)
* [#310](https://github.com/canmod/macpan2/issues/310)
* [#312](https://github.com/canmod/macpan2/issues/312)


## Changes in 1.17.0

Released [2025-05-15]

[source](https://github.com/canmod/macpan2/tree/a99e1b9b6759ac46214c3ecc2bee694113aab88a)
 | [change list](https://github.com/canmod/macpan2/compare/5893d38344b298119bb89a4047ee459193afe2f8..a99e1b9b6759ac46214c3ecc2bee694113aab88a)

### New Features

* More robust handling of C++ log files, even if simulators are loaded from disk to a file system that does not have (or no longer has) the expected path to the log file.


## Changes in 1.16.13

Released [2025-05-15]

[source](https://github.com/canmod/macpan2/tree/5893d38344b298119bb89a4047ee459193afe2f8)
 | [change list](https://github.com/canmod/macpan2/compare/6eaf25f0c9a8a8c4167ac5974ba8675cb021478c..5893d38344b298119bb89a4047ee459193afe2f8)

## Changes in 1.16.12

Released [2025-04-17]

[source](https://github.com/canmod/macpan2/tree/6eaf25f0c9a8a8c4167ac5974ba8675cb021478c)
 | [change list](https://github.com/canmod/macpan2/compare/723b1647840c811da9682dcb28306b79b4aeeeff..6eaf25f0c9a8a8c4167ac5974ba8675cb021478c)

## Changes in 1.16.11

Released [2025-04-04]

[source](https://github.com/canmod/macpan2/tree/723b1647840c811da9682dcb28306b79b4aeeeff)
 | [change list](https://github.com/canmod/macpan2/compare/1d2468ddf09f07888aa80488b3e869a7a8e971d6..723b1647840c811da9682dcb28306b79b4aeeeff)

### New Features

* Transform parameters when making a calibration by prefixing parameter names with the name of the transformation (e.g., `log_beta` instead of `beta`).


## Changes in 1.16.10

Released [2025-04-04]

[source](https://github.com/canmod/macpan2/tree/1d2468ddf09f07888aa80488b3e869a7a8e971d6)
 | [change list](https://github.com/canmod/macpan2/compare/a5d0870b0e06a0a1192db82194b8711dfddcafa5..1d2468ddf09f07888aa80488b3e869a7a8e971d6)

## Changes in 1.16.9

Released [2025-04-04]

[source](https://github.com/canmod/macpan2/tree/a5d0870b0e06a0a1192db82194b8711dfddcafa5)
 | [change list](https://github.com/canmod/macpan2/compare/93061ef0203d500b9764dcda1bffdf940837127a..a5d0870b0e06a0a1192db82194b8711dfddcafa5)

## Changes in 1.16.8

Released [2025-04-04]

[source](https://github.com/canmod/macpan2/tree/93061ef0203d500b9764dcda1bffdf940837127a)
 | [change list](https://github.com/canmod/macpan2/compare/739093e0630e2b6ea993af9a5a05ce428496823a..93061ef0203d500b9764dcda1bffdf940837127a)

## Changes in 1.16.7

Released [2025-03-31]

[source](https://github.com/canmod/macpan2/tree/739093e0630e2b6ea993af9a5a05ce428496823a)
 | [change list](https://github.com/canmod/macpan2/compare/6415a062de47f291973b891e9ee990d6bebf3707..739093e0630e2b6ea993af9a5a05ce428496823a)

## Changes in 1.16.6

Released [2025-03-24]

[source](https://github.com/canmod/macpan2/tree/6415a062de47f291973b891e9ee990d6bebf3707)
 | [change list](https://github.com/canmod/macpan2/compare/958c18f10bd0844f7a43052a8378d5531a09fee9..6415a062de47f291973b891e9ee990d6bebf3707)

## Changes in 1.16.5

Released [2025-03-23]

[source](https://github.com/canmod/macpan2/tree/958c18f10bd0844f7a43052a8378d5531a09fee9)
 | [change list](https://github.com/canmod/macpan2/compare/f97745fe1e12d539f3c1beb851c376828aa274ec..958c18f10bd0844f7a43052a8378d5531a09fee9)

## Changes in 1.16.4

Released [2025-03-21]

[source](https://github.com/canmod/macpan2/tree/f97745fe1e12d539f3c1beb851c376828aa274ec)
 | [change list](https://github.com/canmod/macpan2/compare/17b8774434abc24bd39a4459f2c6fa6775565232..f97745fe1e12d539f3c1beb851c376828aa274ec)

## Changes in 1.16.3

Released [2025-03-20]

[source](https://github.com/canmod/macpan2/tree/17b8774434abc24bd39a4459f2c6fa6775565232)
 | [change list](https://github.com/canmod/macpan2/compare/79fa9543dc4ce70ae9a996cd3784249f8c35c19b..17b8774434abc24bd39a4459f2c6fa6775565232)

## Changes in 1.16.2

Released [2025-03-20]

[source](https://github.com/canmod/macpan2/tree/79fa9543dc4ce70ae9a996cd3784249f8c35c19b)
 | [change list](https://github.com/canmod/macpan2/compare/597b08ae9eea3aa3c40b28d14857c48cc507af1d..79fa9543dc4ce70ae9a996cd3784249f8c35c19b)

## Changes in 1.16.1

Released [2025-03-20]

[source](https://github.com/canmod/macpan2/tree/597b08ae9eea3aa3c40b28d14857c48cc507af1d)
 | [change list](https://github.com/canmod/macpan2/compare/d41b2b103a1886b4f970a29a4510c84ce5e49952..597b08ae9eea3aa3c40b28d14857c48cc507af1d)

## Changes in 1.16.0

Released [2025-03-19]

[source](https://github.com/canmod/macpan2/tree/d41b2b103a1886b4f970a29a4510c84ce5e49952)
 | [change list](https://github.com/canmod/macpan2/compare/5cc2042c42890807af7487f679c72fe5a305988f..d41b2b103a1886b4f970a29a4510c84ce5e49952)

## Changes in 1.15.3

Released [2025-03-17]

[source](https://github.com/canmod/macpan2/tree/5cc2042c42890807af7487f679c72fe5a305988f)
 | [change list](https://github.com/canmod/macpan2/compare/ace5fd2b3ede943385a7c7e50d295aaafe3fb325..5cc2042c42890807af7487f679c72fe5a305988f)

## Changes in 1.15.2

Released [2025-03-11]

[source](https://github.com/canmod/macpan2/tree/ace5fd2b3ede943385a7c7e50d295aaafe3fb325)
 | [change list](https://github.com/canmod/macpan2/compare/5c7a4f3c4df572828d75056fd0c24920ab139c7c..ace5fd2b3ede943385a7c7e50d295aaafe3fb325)

## Changes in 1.15.1

Released [2025-03-11]

[source](https://github.com/canmod/macpan2/tree/5c7a4f3c4df572828d75056fd0c24920ab139c7c)
 | [change list](https://github.com/canmod/macpan2/compare/52cc2768cea6a17c782a6f24fd88febe222c43c2..5c7a4f3c4df572828d75056fd0c24920ab139c7c)

## Changes in 1.15.0

Released [2025-03-03]

[source](https://github.com/canmod/macpan2/tree/52cc2768cea6a17c782a6f24fd88febe222c43c2)
 | [change list](https://github.com/canmod/macpan2/compare/0f0e4eb4683386718f09c7bef79570aeaa310442..52cc2768cea6a17c782a6f24fd88febe222c43c2)

## Changes in 1.14.4

Released [2025-03-02]

[source](https://github.com/canmod/macpan2/tree/0f0e4eb4683386718f09c7bef79570aeaa310442)
 | [change list](https://github.com/canmod/macpan2/compare/0ef07facd2fae06c9a3b1873d0e646e69bc7ba62..0f0e4eb4683386718f09c7bef79570aeaa310442)

## Changes in 1.14.3

Released [2025-02-20]

[source](https://github.com/canmod/macpan2/tree/0ef07facd2fae06c9a3b1873d0e646e69bc7ba62)
 | [change list](https://github.com/canmod/macpan2/compare/34c2502d68624fc6db31c45f4a2aef467c02da53..0ef07facd2fae06c9a3b1873d0e646e69bc7ba62)

## Changes in 1.14.2

Released [2025-02-19]

[source](https://github.com/canmod/macpan2/tree/34c2502d68624fc6db31c45f4a2aef467c02da53)
 | [change list](https://github.com/canmod/macpan2/compare/ac36c86981dea5b5e598aa768cb9dbe3c334bfd0..34c2502d68624fc6db31c45f4a2aef467c02da53)

## Changes in 1.14.1

Released [2025-01-29]

[source](https://github.com/canmod/macpan2/tree/ac36c86981dea5b5e598aa768cb9dbe3c334bfd0)
 | [change list](https://github.com/canmod/macpan2/compare/7bae269ac33375355a469adbb72bca5631ddeaa0..ac36c86981dea5b5e598aa768cb9dbe3c334bfd0)

## Changes in 1.14.0

Released [2025-01-29]

[source](https://github.com/canmod/macpan2/tree/7bae269ac33375355a469adbb72bca5631ddeaa0)
 | [change list](https://github.com/canmod/macpan2/compare/a0b9f30aedde8b8b0ef744643a68777e803f2862..7bae269ac33375355a469adbb72bca5631ddeaa0)

## Changes in 1.13.0

Released [2025-01-13]

[source](https://github.com/canmod/macpan2/tree/a0b9f30aedde8b8b0ef744643a68777e803f2862)
 | [change list](https://github.com/canmod/macpan2/compare/9d793207d18fa3fb66383542717db5ac182ae3a9..a0b9f30aedde8b8b0ef744643a68777e803f2862)

## Changes in 1.12.0

Released [2024-12-30]

[source](https://github.com/canmod/macpan2/tree/9d793207d18fa3fb66383542717db5ac182ae3a9)
 | [change list](https://github.com/canmod/macpan2/compare/bff5dbf5a463f6819437b2b1cc68f1865e5be4c1..9d793207d18fa3fb66383542717db5ac182ae3a9)

## Changes in 1.11.1

Released [2024-11-27]

[source](https://github.com/canmod/macpan2/tree/bff5dbf5a463f6819437b2b1cc68f1865e5be4c1)
 | [change list](https://github.com/canmod/macpan2/compare/19dd287d9c33415fa3a5861b2eef34502b2aa606..bff5dbf5a463f6819437b2b1cc68f1865e5be4c1)

## Changes in 1.11.0

Released [2024-11-15]

[source](https://github.com/canmod/macpan2/tree/19dd287d9c33415fa3a5861b2eef34502b2aa606)
 | [change list](https://github.com/canmod/macpan2/compare/59c5cf8a5acc5689b9e2ff590f937bfffc5582a5..19dd287d9c33415fa3a5861b2eef34502b2aa606)

## Changes in 1.10.0

Released [2024-11-06]

[source](https://github.com/canmod/macpan2/tree/59c5cf8a5acc5689b9e2ff590f937bfffc5582a5)
 | [change list](https://github.com/canmod/macpan2/compare/a865e9ea2ea6669e58d14c6d7a069e93a1fb6a15..59c5cf8a5acc5689b9e2ff590f937bfffc5582a5)

## Changes in 1.9.1

Released [2024-10-11]

[source](https://github.com/canmod/macpan2/tree/a865e9ea2ea6669e58d14c6d7a069e93a1fb6a15)
 | [change list](https://github.com/canmod/macpan2/compare/090ae073179fffaef743e73bc9668112f00895a4..a865e9ea2ea6669e58d14c6d7a069e93a1fb6a15)

## Changes in 1.9.0

Released [2024-10-08]

[source](https://github.com/canmod/macpan2/tree/090ae073179fffaef743e73bc9668112f00895a4)
 | [change list](https://github.com/canmod/macpan2/compare/19ac6084925a6b84c79d3588ba48e8d3ef933822..090ae073179fffaef743e73bc9668112f00895a4)

## Changes in 1.8.1

Released [2024-10-05]

[source](https://github.com/canmod/macpan2/tree/19ac6084925a6b84c79d3588ba48e8d3ef933822)
 | [change list](https://github.com/canmod/macpan2/compare/3b420e6c08ea2980d7b6279c9c1b5fb96a5db1a9..19ac6084925a6b84c79d3588ba48e8d3ef933822)

## Changes in 1.8.0

Released [2024-09-27]

[source](https://github.com/canmod/macpan2/tree/3b420e6c08ea2980d7b6279c9c1b5fb96a5db1a9)
 | [change list](https://github.com/canmod/macpan2/compare/25f60907411bdd9c3c8afb8551f34746ab4e55fc..3b420e6c08ea2980d7b6279c9c1b5fb96a5db1a9)

## Changes in 1.7.2

Released [2024-09-25]

[source](https://github.com/canmod/macpan2/tree/25f60907411bdd9c3c8afb8551f34746ab4e55fc)
 | [change list](https://github.com/canmod/macpan2/compare/e057fc11f6ded4d0fb827c69e3eeb6bd7a933f75..25f60907411bdd9c3c8afb8551f34746ab4e55fc)

## Changes in 1.7.1

Released [2024-09-10]

[source](https://github.com/canmod/macpan2/tree/e057fc11f6ded4d0fb827c69e3eeb6bd7a933f75)
 | [change list](https://github.com/canmod/macpan2/compare/f56593bb3628fe75310cd4199ee33e227ddc3ec7..e057fc11f6ded4d0fb827c69e3eeb6bd7a933f75)

## Changes in 1.7.0

Released [2024-08-29]

[source](https://github.com/canmod/macpan2/tree/f56593bb3628fe75310cd4199ee33e227ddc3ec7)
 | [change list](https://github.com/canmod/macpan2/compare/544eada2c8b2b19b06f53c5798e39dd24e756363..f56593bb3628fe75310cd4199ee33e227ddc3ec7)

## Changes in 1.6.1

Released [2024-08-28]

[source](https://github.com/canmod/macpan2/tree/544eada2c8b2b19b06f53c5798e39dd24e756363)
 | [change list](https://github.com/canmod/macpan2/compare/0adeebc4ac152b81ef876ee31636fc0993eb871c..544eada2c8b2b19b06f53c5798e39dd24e756363)

## Changes in 1.6.0

Released [2024-08-27]

[source](https://github.com/canmod/macpan2/tree/0adeebc4ac152b81ef876ee31636fc0993eb871c)
 | [change list](https://github.com/canmod/macpan2/compare/c738f45cde4fd6f9a48e569c65f046cfd0decc01..0adeebc4ac152b81ef876ee31636fc0993eb871c)

## Changes in 1.5.6

Released [2024-08-20]

[source](https://github.com/canmod/macpan2/tree/c738f45cde4fd6f9a48e569c65f046cfd0decc01)
 | [change list](https://github.com/canmod/macpan2/compare/97b81cb41fbd8032f3aeb91b1cd125ed38fe0686..c738f45cde4fd6f9a48e569c65f046cfd0decc01)

## Changes in 1.5.5

Released [2024-06-23]

[source](https://github.com/canmod/macpan2/tree/97b81cb41fbd8032f3aeb91b1cd125ed38fe0686)
 | [change list](https://github.com/canmod/macpan2/compare/7db44311892e35b4bef5ca6b4bf9e78f979c01fe..97b81cb41fbd8032f3aeb91b1cd125ed38fe0686)

## Changes in 1.5.4

Released [2024-06-19]

[source](https://github.com/canmod/macpan2/tree/7db44311892e35b4bef5ca6b4bf9e78f979c01fe)
 | [change list](https://github.com/canmod/macpan2/compare/f944bbfbf528a50e48a300180d858663156d049b..7db44311892e35b4bef5ca6b4bf9e78f979c01fe)

## Changes in 1.5.3

Released [2024-06-17]

[source](https://github.com/canmod/macpan2/tree/f944bbfbf528a50e48a300180d858663156d049b)
 | [change list](https://github.com/canmod/macpan2/compare/5518ef01c5d9460378a99beefcbfcb4029d97bec..f944bbfbf528a50e48a300180d858663156d049b)

## Changes in 1.5.2

Released [2024-06-10]

[source](https://github.com/canmod/macpan2/tree/5518ef01c5d9460378a99beefcbfcb4029d97bec)
 | [change list](https://github.com/canmod/macpan2/compare/8b7e63eda6a7250b6d88018f172756817614b291..5518ef01c5d9460378a99beefcbfcb4029d97bec)

## Changes in 1.5.1

Released [2024-06-12]

[source](https://github.com/canmod/macpan2/tree/8b7e63eda6a7250b6d88018f172756817614b291)
 | [change list](https://github.com/canmod/macpan2/compare/10bb06f4ac7673f2cb7e509ac2cff40793662917..8b7e63eda6a7250b6d88018f172756817614b291)

## Changes in 1.5.0

Released [2024-05-30]

[source](https://github.com/canmod/macpan2/tree/10bb06f4ac7673f2cb7e509ac2cff40793662917)
 | [change list](https://github.com/canmod/macpan2/compare/77cacc01f53409a729243236320c0271b34081ed..10bb06f4ac7673f2cb7e509ac2cff40793662917)

### New Features

* Delete elements from model specifications with `mp_tmb_delete`.


## Changes in 1.4.1

Released [2024-05-16]

[source](https://github.com/canmod/macpan2/tree/77cacc01f53409a729243236320c0271b34081ed)
 | [change list](https://github.com/canmod/macpan2/compare/544e1d4f1fc4c7efe3d92842b2018f531398e435..77cacc01f53409a729243236320c0271b34081ed)

## Changes in 1.4.0

Released [2024-04-25]

[source](https://github.com/canmod/macpan2/tree/544e1d4f1fc4c7efe3d92842b2018f531398e435)
 | [change list](https://github.com/canmod/macpan2/compare/a45472a350520f00a18b248622c17a766130dbad..544e1d4f1fc4c7efe3d92842b2018f531398e435)

### Behaviour Changes

* Repeated calls of an optimizer now start from the previous best parameter
vector
* No longer fit full covariance matrix in `sdreport`s

### Bug Fixes

* Row vectors with names no longer break spec print methods


## Changes in 1.3.3

Released [2024-04-18]

[source](https://github.com/canmod/macpan2/tree/a45472a350520f00a18b248622c17a766130dbad)
 | [change list](https://github.com/canmod/macpan2/compare/7cead91444f2525a17db769fc979be2313c4bc09..a45472a350520f00a18b248622c17a766130dbad)

## Changes in 1.3.2

Released [2024-04-08]

[source](https://github.com/canmod/macpan2/tree/7cead91444f2525a17db769fc979be2313c4bc09)
 | [change list](https://github.com/canmod/macpan2/compare/24cee41f0a87f1bc25f86e979f2b8184d6c891a5..7cead91444f2525a17db769fc979be2313c4bc09)

## Changes in 1.3.1

Released [2024-03-12]

[source](https://github.com/canmod/macpan2/tree/24cee41f0a87f1bc25f86e979f2b8184d6c891a5)
 | [change list](https://github.com/canmod/macpan2/compare/ee9b0025d3792ea27a55d441c17e7ca57af8653a..24cee41f0a87f1bc25f86e979f2b8184d6c891a5)

### Behaviour Changes

* Removing in-place modifications to model specification objects in `mp_tmb_insert` and `mp_tmb_update`.

### New Features

* Define explicit state variable updates, with the choice of applying `mp_euler`,
`mp_rk4`, or `mp_euler_multinomial` update methods, the latter generating 
process error.
* New stochasticity engine functions `rbinom` and `reulermultinom`.
* `mp_tmb_fixef_cov` function for getting the covariance matrix of fixed effects.
* `mp_trajectory_ensemble` and `mp_trajectory_sim` functions for summarizing random variation in trajectories.
* Unpacking assignment in the engine. This means that you can do things like `c(x, y) ~ z`, where the values in `z` are assigned to elements in `x`, `y`, etc... in row-major order.
* `to_name_pairs` function for returning all pairwise dot-concatenations of the elements of a character vector without dots.
* `to_values` function for extracting the column from a data frame with only a single numerical column and turning numeric matrices and arrays with dimnames into a flattened numeric vector with labels produced by appropriately dot-concatenating the dimnames.

### Bug Fixes

* Several bugs related to input handling in `mp_tmb_calibrator` (#176).

### Doc Fixes and Updates

* New installation instructions for installing from `r-universe`.
* New vignette: `real_data`.
* Document `to_name`, `to_names`, `to_labels`, which handle naming of structured objects.
* Document `print` function in the `?engine_functions`.
* `simple_sims` example in `?engine_functions` now runs without error.
* Help file examples for `mp_tmb_insert` and `mp_tmb_update`.
* Readme/vignette examples better expose calls to `library` for the user.


## Changes in 1.3.0

Released [2024-03-11]

[source](https://github.com/canmod/macpan2/tree/ee9b0025d3792ea27a55d441c17e7ca57af8653a)
 | [change list](https://github.com/canmod/macpan2/compare/a1d4e89a2a47abff98bde068bf514a5ee0ed834d..ee9b0025d3792ea27a55d441c17e7ca57af8653a)

## Changes in 1.2.1

Released [2024-03-05]

[source](https://github.com/canmod/macpan2/tree/a1d4e89a2a47abff98bde068bf514a5ee0ed834d)
 | [change list](https://github.com/canmod/macpan2/compare/3820ab36795d435613b91b4032cbeb7fabda1ad3..a1d4e89a2a47abff98bde068bf514a5ee0ed834d)

## Changes in 1.2.0

Released [2024-03-11]

[source](https://github.com/canmod/macpan2/tree/3820ab36795d435613b91b4032cbeb7fabda1ad3)
 | [change list](https://github.com/canmod/macpan2/compare/291064c37c2ab64f42fe206284d350c5e50d8968..3820ab36795d435613b91b4032cbeb7fabda1ad3)

## Changes in 1.1.3

Released [2024-03-01]

[source](https://github.com/canmod/macpan2/tree/291064c37c2ab64f42fe206284d350c5e50d8968)
 | [change list](https://github.com/canmod/macpan2/compare/e89b834c92eab5efb5332aa29d7e47ae4a3794a5..291064c37c2ab64f42fe206284d350c5e50d8968)

## Changes in 1.1.2

Released [2024-02-26]

[source](https://github.com/canmod/macpan2/tree/e89b834c92eab5efb5332aa29d7e47ae4a3794a5)
 | [change list](https://github.com/canmod/macpan2/compare/4c9e6a207d13e752937fc69c1c2a136f509b02fc..e89b834c92eab5efb5332aa29d7e47ae4a3794a5)

## Changes in 1.1.1

Released [2024-02-25]

[source](https://github.com/canmod/macpan2/tree/4c9e6a207d13e752937fc69c1c2a136f509b02fc)
 | [change list](https://github.com/canmod/macpan2/compare/b2b8256520ffee4a458cd5b4f61763ec9bb56245..4c9e6a207d13e752937fc69c1c2a136f509b02fc)

## Changes in 1.1.0

Released [2024-02-23]

[source](https://github.com/canmod/macpan2/tree/b2b8256520ffee4a458cd5b4f61763ec9bb56245)
 | [change list](https://github.com/canmod/macpan2/compare/c8168efa30b4893a831730854009a7a4dbfe3c61..b2b8256520ffee4a458cd5b4f61763ec9bb56245)

### Behaviour Changes

* `simple_sims` no longer returns outputs for the zeroth time-step.
* Using `last.par.best` to finalize the TMB objective function following optimization.
* `group_sums` now checks for bad group indexes.
* `rbind_lag` now throws an error when `lag > 1` because there are conceptual errors with this case. We will get back to this when we can to allow these important cases by adding an argument with initial conditions associated with negative time steps.

### New Features

* Streamlined installation via `r-universe`.
* `mp_tmb_calibrator` and `mp_tmb_optimize` functions for calibration using a simple and restrictive trajectory matching model.  Future releases will allow more functionality.
* `mp_tmb_coef` and `mp_tmbstan_coef` for tables of statistical parameters used in calibration.
* `mp_trajectory_sd` and `mp_trajectory_ensemble` for getting information about fitted trajectory uncertainty.
* `mp_tmb_update|insert` functions for creating new model specs from existing ones.
* Parameters specified as a data frame can now place default values in columns with any of the following names: `"default", "Default", "value", "Value", "val", "Val"`
* `mp_tmb_library` can return a list of model specs with `alternative_specs`, if the model makes alternatives available.
* `time_var` engine function is better than the old `time_group`, which required two expressions rather than one to do the same thing. `time_group` will remain but is softly deprecated.
* Fixed effects extractor and formatter.
* `mp_default` function for extracting spec and simulator defaults in long-format.
* `rbind_time` allows integer vectors for subsetting times
* `options(macpan2_verbose = FALSE)` will turn off the flood of information provided by `TMB`. Note that this only takes effect if set before creating a TMB simulator.

### Bug Fixes

* Segfaults for out-of-range assignment.

### Doc Fixes and Updates

* (in progress) [Calibration vignette](https://github.com/canmod/macpan2/blob/HEAD/vignettes/calibration.Rmd) is updated to be a simpler quick-start, and previous advanced material is moved to an advanced vignette.
* `mp_tmb_model_spec` documentation is filled out.
* `group_sums` TMB engine function third argument updated from old pre-1.0.0 behaviour.


## Changes in 1.0.2

Released [2024-02-20]

[source](https://github.com/canmod/macpan2/tree/c8168efa30b4893a831730854009a7a4dbfe3c61)
 | [change list](https://github.com/canmod/macpan2/compare/77de45a92c8861fed54c4016056c03f42db86953..c8168efa30b4893a831730854009a7a4dbfe3c61)

## Changes in 1.0.1

Released [2024-02-20]

[source](https://github.com/canmod/macpan2/tree/77de45a92c8861fed54c4016056c03f42db86953)
 | [change list](https://github.com/canmod/macpan2/compare/4756a64a26fefb26fa21c41ee23628e51d4eda65..77de45a92c8861fed54c4016056c03f42db86953)

## Changes in 1.0.0

Released [2024-02-18]

[source](https://github.com/canmod/macpan2/tree/4756a64a26fefb26fa21c41ee23628e51d4eda65)
 | [change list](https://github.com/canmod/macpan2/compare/9bf7fc757ea54be599b42cd08ddd9ec2a124f491..4756a64a26fefb26fa21c41ee23628e51d4eda65)

### Behaviour Changes

* No more `flows.csv`, `derivations.json` files in the library, and instead `tmb.R` files.
* No more `Compartmental` function, and instead `mp_tmb_model_spec` and `mp_tmb_library`.
* `{group|row|col}Sums` are now called `{group|row|col}_sums`.
* Final argument of `group_sums` used to be the length of the output vector, but now it is a vector of the desired output length.
* `TMBModel` and associated classes (`ExprList`) are no longer exported, in favour of `mp_...` functions for doing similar things.
* Sort simulation output by time step.

### New Features

* Constant integer vectors can now be passed to `C++`.
* Integer vectors can be used to subset assignment matrices (i.e. integer subsetting on the left-hand-side).
* `mp_trajectory()` function, which is roughly equivalent to `model$report(..., .phases = "during")`.
* New `print` function in the TMB engine.
* No need to declare empty matrices when creating TMB model objects, and matrices that are derived are automatically detected.

### Experimental Features

* Model structure grammar (`mp_index()`, `mp_join()`, etc.)
* Log files


## Changes in 0.0.4

Released [2024-01-18]

[source](https://github.com/canmod/macpan2/tree/9bf7fc757ea54be599b42cd08ddd9ec2a124f491)
 | [change list](https://github.com/canmod/macpan2/compare/bdc323d423563d7ac7432a81e1516831cd966960..9bf7fc757ea54be599b42cd08ddd9ec2a124f491)

## Changes in 0.0.3

Released [2023-10-25]

[source](https://github.com/canmod/macpan2/tree/bdc323d423563d7ac7432a81e1516831cd966960)
 | [change list](https://github.com/canmod/macpan2/compare/b47bc9371d4025f9d0bd712e3628bf4d846cb7f0..bdc323d423563d7ac7432a81e1516831cd966960)

* Optimize C++ simulation history storage by avoiding unnecessary allocations
* Use state and flow names in expression inserters
* Chattier validity checking
* Radial basis functions
* New starter models (thanks @mayaearn and @Flynn-Primrose )
    * `macpan-base` -- re-implementation of the McMaster group's COVID-19 model in `macpan2`
    * `ww` -- wastewater model (doesn't yet have a readme)
    * new readme for and clean up of previous models
* Report what expression broke on the C++ side
* Developer tools for switching between different C++ files and working directories
* Package reference organization cleanup (thanks @bbolker )
* Time-varying parameters vignette


## Changes in 0.0.2

Released [2023-06-06]

[source](https://github.com/canmod/macpan2/tree/b47bc9371d4025f9d0bd712e3628bf4d846cb7f0)
 | [change list](https://github.com/canmod/macpan2/compare/9d81208ae329d772f68687ba8d0a2ac228947dff..b47bc9371d4025f9d0bd712e3628bf4d846cb7f0)

* Interface for optimization of TMB simulation objects
* TMB simulation model updating with caching
* Parameter transformations
* Get initial values of matrices in TMB simulation objects
* State and flow variable names can be used in expressions in some contexts
* Example model indexing (thank you @bbolker!)
* Engine function rbind_time defaults to row binding the full simulation history
* Fix bug when the entire model has no inflows or no outflows
* Fix bugs in symbolic R-side manipulation of expressions
* Fix previously broken argument_dots option in model definition files (thank you @Flynn-Primrose )


## Changes in 0.0.1

Released [2023-05-26]

[source](https://github.com/canmod/macpan2/tree/9d81208ae329d772f68687ba8d0a2ac228947dff)
 | [change list](https://github.com/canmod/macpan2/compare/ded98a20184b9e382521472a8de90951a6cc3359..9d81208ae329d772f68687ba8d0a2ac228947dff)

* Initial release.

