# Changelog

## 1.0.0 (2025-08-06)


### ⚠ BREAKING CHANGES

* Fixed 'missing implementation' issue in mean and median functions

### fix\

* Fixed 'missing implementation' issue in mean and median functions ([8d3ebb7](https://github.com/dnpm-dip/service-base/commit/8d3ebb75c447456729bbad4a59c07c783c02f9ec))


### Features

* Adaptations/Improvements for MVH ([3ae02a7](https://github.com/dnpm-dip/service-base/commit/3ae02a7caa0f91da988142b86864a8e363e93498))
* Adapted Orchestrator; bit of refactoring of MVH data structures ([5489123](https://github.com/dnpm-dip/service-base/commit/5489123047daf355bb24d6bd4435e5662be0eb62))
* Added defaultFilter method to ResultSet (but kept the default filter on Query to keep the API non-breaking for now) ([c7677df](https://github.com/dnpm-dip/service-base/commit/c7677dfe59a05b0ba9991beadd2ef4395f304f1f))
* Added missing attributes to Submission.Report; Adapaed Orchestrator to only forward submissions to MVH service when validation passed ([a0091ce](https://github.com/dnpm-dip/service-base/commit/a0091ce844daf632e27e3539c08058b56f29bc90))
* Added MVH-specific extensions methods for PatientRecord; Adapted Validator using those extension methods ([0de9dff](https://github.com/dnpm-dip/service-base/commit/0de9dffd8559a01056a80d9d1aad3b7cb261770b))
* Added new MVH submission type 'test' ([59b9974](https://github.com/dnpm-dip/service-base/commit/59b9974a4e5c782b0117bc1c844266e7345a7752))
* Added service status info; Moved Entry, Distribution up from 'query' sub-package ([72eb031](https://github.com/dnpm-dip/service-base/commit/72eb03151e201cb2083999906c1c4cf6ac54b782))
* Added some validators for use in derived components ([292d5f8](https://github.com/dnpm-dip/service-base/commit/292d5f8b5ed05edc2dad6fd4706f2121572cc588))
* Added Submission.Report.sequencingType, since the issue reported on BfArM repo that this shouldn't pertain to the clinical data report has been ignored so far; Removed payload from successful upload command response ([e91e40f](https://github.com/dnpm-dip/service-base/commit/e91e40f3c087250ae282d790b737fafff865825b))
* Added ValidationInfo.createdAt and sorting in decreasing order of issue number; Added persistent store for prepared queries ([40779ed](https://github.com/dnpm-dip/service-base/commit/40779ed08b576cd00970b0ee296d1e4083be5013))
* Added validators for MVH Metadata and DataUpload; Adaptations to impacted components ([9e0b071](https://github.com/dnpm-dip/service-base/commit/9e0b071d6de156a8e41544481763bccf27e46482))
* Improvements to base validators; Upgrade of scalatest version ([6eec179](https://github.com/dnpm-dip/service-base/commit/6eec1791547a36b721f69456d347fddd33023e9a))
* Refactored ConnectionStatus usage; (minor) refactoring of Submission to remove status history ([047041b](https://github.com/dnpm-dip/service-base/commit/047041b8269e0cefa3c5241126eed9b67aa52b45))
* Refactored PatientFilter: moved predicate method to extension method for cleaner separation of PatientFilter as DTO and as predicate function ([b658bb6](https://github.com/dnpm-dip/service-base/commit/b658bb6042a21e4b38ea044417b3d5f46ab60b43))
* Refactoring to MVH specs ([901ccbb](https://github.com/dnpm-dip/service-base/commit/901ccbbd955908bf5af3ff8751f16a6c08380f14))
* Work on modelling MVH metadata, esp. consent ([abb9463](https://github.com/dnpm-dip/service-base/commit/abb94630b0f29130005a0d17c41770bdffe4a245))
* Work on MVH Submission.Report handling ([88bee8d](https://github.com/dnpm-dip/service-base/commit/88bee8d70943a2a1b4478fddfd8d0be4b18791a3))
* Worked on Submission.Report generation, with corresponding adaptations to Orchestrator; Added regex validation rules for Study-Ids ([0aba98d](https://github.com/dnpm-dip/service-base/commit/0aba98dee0d32a3de7b6cbf8e0b8b8a32bc0a2b6))


### Bug Fixes

* Adaptation to refactored PatientRecord base ([aafc258](https://github.com/dnpm-dip/service-base/commit/aafc258f3c8127e2596603a347114bb10d01afb4))
* Adapted base PatientRecord validator to refactored base CarePlan ([5d0962c](https://github.com/dnpm-dip/service-base/commit/5d0962c7a23dc375e9f6f510696d9db82649efcb))
* Adapted BaseMVHService and Extensions to use new implicit conversions for Code[NGSReport.Type.Value]; Increased severity level of missing sequencing consent to 'fatal' ([9942aa9](https://github.com/dnpm-dip/service-base/commit/9942aa9c1a9300f67af258a2d8c1ad5aa699ea83))
* Adapted Patient validator to optional address ([e862fe6](https://github.com/dnpm-dip/service-base/commit/e862fe64790ffe55e15365b9c32d73d935d2e774))
* Adapted scalac linting and fixed many reported errors (mostly unused imports) ([2822ee5](https://github.com/dnpm-dip/service-base/commit/2822ee578d0f1e58fedd3aae05c1724d149a334b))
* Adapted validation check that either a sequencing report requested in the MVH context or that the reason for non-sequencing must exist (be defined) ([0cce6e6](https://github.com/dnpm-dip/service-base/commit/0cce6e6b6484e720868959114a54454aa20b8046))
* Adapted validation to check that sequencing reports containing variant findings occur, not just that (possibly empty) reports occur when MVH sequencing was not rejected ([925b638](https://github.com/dnpm-dip/service-base/commit/925b638cfc80efb445404ee3e18b986ff396bb3f))
* Added check to ensure that TANs haven't been used already ([99d4889](https://github.com/dnpm-dip/service-base/commit/99d4889f1dd001102022ef3fbe39725c5b8a92c8))
* Added fallback value for Study-ID validation patterns; Removed check for presence of therapyLine on Therapy base validator ([a31d81a](https://github.com/dnpm-dip/service-base/commit/a31d81addfed001b1ced7e4480fa3d543eb0d736))
* Added more logging output for better error diagnosis in repository components backed by file-system ([f8cc870](https://github.com/dnpm-dip/service-base/commit/f8cc87043642b4143ca07e122e8b9772b86016e0))
* code clean-up ([1d14a0e](https://github.com/dnpm-dip/service-base/commit/1d14a0ec83a3577d017dd5922a4c9a493f627b71))
* Fixed bug due to erroneous pattern match in Orchestrator ([3c674bf](https://github.com/dnpm-dip/service-base/commit/3c674bffc6b4f1864bfe27a9a042b06f00286757))
* Fixed bug in Issue.Path parsing that led to artifact empty entries and thus double slashes upon subsequent String serialization of the path ([c2b6b16](https://github.com/dnpm-dip/service-base/commit/c2b6b16496d439290dfdcd43b69e811cba6e8754))
* Fixed bug in JSON Schemea generation for Submission.Type ([e7193a3](https://github.com/dnpm-dip/service-base/commit/e7193a3635a15204c6edefff24e40ebb1f59c933))
* Fixed bug in Path parsing ([ea48f81](https://github.com/dnpm-dip/service-base/commit/ea48f815735c495abe55bec573a8553c863c2639))
* Fixed bug in pattern matching on save result ([abc7ea5](https://github.com/dnpm-dip/service-base/commit/abc7ea5a077b92066d668b7059e5a3bc163138c2))
* Fixes/improvements to base validators ([d37c431](https://github.com/dnpm-dip/service-base/commit/d37c4316fb701e0faa3ea0a244ae6ce535edd415))
* Made Path parsing test more robust ([6fe81ed](https://github.com/dnpm-dip/service-base/commit/6fe81ed5117b377a702c9abdd01452c112fa9643))
