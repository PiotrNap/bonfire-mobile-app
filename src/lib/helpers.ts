import { Platform, Share } from "react-native"

import { ANDROID_API_URL, IOS_API_URL } from "@env"
import { signChallenge } from "./tweetnacl"
import Aes from "react-native-aes-crypto"
import Crypto from "crypto"
import base64 from "base64-js"

import { monthsByName } from "common/types/calendarTypes"
import { Auth } from "../services/Api/Auth"
import { getDeepLinkUri } from "./utils"
import { createNestedPath } from "./navigation"
import { DEEP_LINKING_URLS } from "common/types/navigationTypes"
import Toast from "react-native-toast-message"

const DEFAULT_ERROR_MSG = "Something went wrong. Please reload the app and try again."

export const isAndroid = Platform.OS === "android"
export const isIOS = Platform.OS === "ios"

/**
 *  Takes index of the selected day in the weeek
 *  and returns recurring days of the same index in current month.
 *
 *  For example:
 *   - Sunday (1-08-2021)
 *   - Sunday (8-08-2021)
 */
export const getRecurringMonthDays = (index: number, year: number, month: string) => {
  const daysArray: number[] = []

  const numOfDays = new Date(year, monthsByName[month] + 1, 0).getDate()
  const firstDayOfWeek = new Date(year, monthsByName[month]).getDay()

  var firstDayToSelect =
    firstDayOfWeek > index ? 7 - firstDayOfWeek + index + 1 : 1 + (index - firstDayOfWeek)
  // Calculate number of weeks (+1 because starting from current selected day)
  var numOfWeeks = Math.floor((numOfDays - firstDayToSelect) / 7 + 1)

  for (; numOfWeeks > 0; numOfWeeks--) {
    daysArray.push(new Date(year, monthsByName[month], firstDayToSelect).getTime())
    firstDayToSelect = firstDayToSelect + 7
  }

  return daysArray
}

/**
 * Starts challenge sequence to obtain JWT from the server.
 * Returns {id, username, accessToken, expiresIn , ???} or  `null` if failed.
 */
export const startChallengeSequence = async (
  privKey: string,
  deviceID: string,
  id: string // user-ID
): Promise<{ [index: string]: string } | void> => {
  let res = await Auth.requestChallenge({
    deviceID,
    id,
  })
  let { challenge } = res

  if (challenge) {
    let signature: any = await signChallenge(
      base64.toByteArray(challenge),
      base64.toByteArray(privKey)
    )
    if (signature) {
      let res = await Auth.requestAccessToken({
        challenge,
        signature,
        id,
        deviceID,
      })
      if (res) return res
    }
  }
  privKey = ""
}

export const getApiUrl = (url: string): string => {
  if (url[0] !== "/") url = "/" + url
  const baseUrl = Platform.OS === "ios" ? IOS_API_URL : ANDROID_API_URL

  return baseUrl + url
}

export const getFormDataFromFilePath = (filePath: string) => {
  const fileChunks = filePath.split(".")
  const fileType = fileChunks[fileChunks.length - 1]
  const formData = new FormData()

  formData.append("file", {
    uri: Platform.OS === "ios" ? filePath.replace("file://", "") : filePath,
    name: `photo.${fileType}`,
    type: `image/${fileType}`,
  })

  return formData
}

export const shareEvent = async (id: string) => {
  try {
    const result = await Share.share({
      message:
        getDeepLinkUri(
          createNestedPath([
            DEEP_LINKING_URLS.NAVIGATION,
            DEEP_LINKING_URLS.NAVIGATION,
            DEEP_LINKING_URLS.BROWSE,
          ])
        ) + `/${id}`,
    })
    if (result.action === Share.sharedAction) {
      if (result.activityType) {
        // shared with activity type of result.activityType
      } else {
        // shared
      }
    } else if (result.action === Share.dismissedAction) {
    }
  } catch (e) {}
}

export const isUUID = (val: string): boolean => /((\w{4,12}-?)){5}/.test(val)

export const encryptWithPassword = async (
  value: string,
  password: string
): Promise<undefined | string> => {
  const salt = Crypto.randomBytes(16)
  const nonce = Crypto.randomBytes(12)
  const base64Value = Buffer.from(value).toString("base64")
  const key = await Aes.pbkdf2(password, salt.toString("hex"), 5000, 256, "sha512")

  const encrypted = await Aes.encrypt(
    base64Value,
    key,
    nonce.toString("hex"),
    "aes-256-ctr"
  )

  // Concatenating salt, nonce, and encrypted data
  return salt.toString("hex") + nonce.toString("hex") + encrypted
}

export const decryptWithPassword = async (
  cipherText: string,
  password: string
): Promise<undefined | string> => {
  // Extracting salt, nonce, and encrypted data from cipherText
  const salt = Buffer.from(cipherText.slice(0, 32), "hex")
  const nonce = Buffer.from(cipherText.slice(32, 56), "hex")
  const encrypted = cipherText.slice(56)

  const key = await Aes.pbkdf2(password, salt.toString("hex"), 5000, 256, "sha512")

  const decryptedBase64 = await Aes.decrypt(
    encrypted,
    key,
    nonce.toString("hex"),
    "aes-256-ctr"
  )

  return Buffer.from(decryptedBase64, "base64").toString()
}

export function showSuccessToast(header: string, body: string): void {
  Toast.show({ text1: header, text2: body })
}

export function showErrorToast(e?: any, header?: string): void {
  const isDev = typeof __DEV__ === "boolean" && __DEV__
  if (isDev) console.error("From Toast: ", e)

  const body = e.message || e.msg || DEFAULT_ERROR_MSG

  //@TODO send possible erorr to Sentry
  Toast.show({
    type: "error",
    text1: header || "Error",
    text2: body,
  })
}
