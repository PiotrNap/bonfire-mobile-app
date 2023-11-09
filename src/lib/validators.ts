import * as yup from "yup"
import { Crypto } from "@hyperionbt/helios"
import { lovelaceToAda } from "./wallet/utils"

const { verifyBech32 } = Crypto

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

export function txSendValidationSchema(lovelaceBalance: bigint) {
  return yup.object().shape({
    receivingAddress: yup
      .string()
      .test("address-verify", "Wrong address", (value) => verifyBech32(value || ""))
      .required("Address is required"),
    ada: yup
      .string()
      .matches(/^[+-]?\d+(\.\d+)?$/, "This input can only contain numbers")
      .test(
        "ada-amount-verify",
        `Max amount is ${lovelaceToAda(lovelaceBalance)}`,
        (val) => (!val ? true : Number(val) < Number(lovelaceBalance) / 1_000_000)
      ),
    // .min(params.shelleyGenesis.protocolParams.minUTxOValue),
  })
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

export function newEventScheme() {
  return yup.object().shape({
    title: yup.string().required("Title is required"),
    ada: yup
      .string()
      .matches(/^[+-]?\d+(\.\d+)?$/, "This input can only contain numbers"),
  })
}

/**
 * @description This will specify validation schema for creating a new
 * account or updating an existing one.
 */
export function accountValidationScheme() {
  return yup.object().shape({
    username: yup
      .string()
      .min(2, "User name is to short (minimum 2 characters)")
      .required("Username is required"),
  })
}

export function hourlyRateValidationScheme() {
  return yup.object().shape({
    ada: yup
      .string()
      .matches(/^[+-]?\d+(\.\d+)?$/, "This input can only contain numbers"),
  })
}

/**
 * @description Validation schema for authenticating user for access to stored wallet keys
 */
export function passwordValidationSchema() {
  return yup.object().shape({
    password: yup
      .string()
      .required("Password is required")
      .matches(/\d/, "Must contain number")
      .min(10, "Min. 10 characters are required"),
  })
}

/**
 * @description Validation schema for setting up spending password
 */
export function passwordSetUpValidationScheme() {
  return yup.object().shape({
    password: passwordValidationSchema(),
    password_confirm: yup
      .string()
      .required("Confirmation is required")
      .oneOf([yup.ref("password")], "Passwords do not match"),
  })
}
