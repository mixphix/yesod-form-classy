# Revision history for yesod-form-classy

## 0.0.0

* Type classes for working with HTML forms in Yesod
  * `data Optionality` and `data Multiplicity` used at the kind level to determine the `FormShape` of the result
  * `mopt` and `mreq` unified into `input`, taking type parameters instead
  * `select` takes type parameters and an additional `OptionList`, and can be used for types without their own `InputField` instance
