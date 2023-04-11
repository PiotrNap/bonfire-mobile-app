import * as yup from "yup"
/**
 * @description Use this function to validate form input that
 * has to contain only numbers
 *
 * @param val
 */
export function validateNumberInput(val: string): boolean {
  var regex = /^\d+$/
  if (+val || val == "") {
    if (regex.test(val)) {
      return true
    }
  }
  return false
}

/**
 * @description This will specify schema for validating organizer
 * form inputs values.
 */
export function formValidationSchema() {
  return yup.object().shape({
    alias: yup.string().required("Alias is required"),
    aboutURL: yup.string().url(),
    imageURL: yup.string().url(),
    timeBlockCostADA: yup
      .string()
      .matches(/^[+-]?\d+(\.\d+)?$/, "This input can only contain numbers")
      .required("Please provide the price"),
    timeBlockLengthMin: yup
      .string()
      .matches(/^[+-]?\d+(\.\d+)?$/, "This input can only contain numbers")
      .required("Please provide the time length"),
  })
}

/**
 * @description This will specify validation schema for creating a new
 * account or updating an existing one.
 */
export function accountValidationScheme() {
  return yup.object().shape({
    name: yup.string().required("Name is required"),
    username: yup
      .string()
      .min(2, "User name is to short (minimum 2 characters)")
      .required("Username is required"),
    ada: yup
      .string()
      .matches(/^[+-]?\d+(\.\d+)?$/, "This input can only contain numbers"),
    gimbals: yup
      .string()
      .matches(/^[+-]?\d+(\.\d+)?$/, "This input can only contain numbers"),
  })
}

/** @description Validation schema for setting up new Wallet name and spending password
 * */
export function walletSetUpValidationScheme() {
  return yup.object().shape({
    name: yup.string().required("Wallet name is required"),
    password: yup
      .string()
      .required("Password is required")
      .min(6, "Min. 6 characters are required"),
    password_confirm: yup
      .string()
      .required("Password confirmation is required")
      .oneOf([yup.ref("password")], "Passwords do not match"),
  })
}