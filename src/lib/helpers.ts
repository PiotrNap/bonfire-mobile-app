import { Platform, Share } from "react-native"

import { ANDROID_API_URL, IOS_API_URL } from "@env"
import { signChallenge } from "./tweetnacl"
import {
  decrypt_with_password,
  encrypt_with_password,
} from "@emurgo/csl-mobile-bridge"
import crs from "crypto-random-string"
import base64 from "base64-js"

import { monthsByName } from "common/types/calendarTypes"
import { Auth } from "../services/Api/Auth"
import { getDeepLinkUri } from "./utils"
import { createNestedPath } from "./navigation"
import { DEEP_LINKING_URLS } from "common/types/navigationTypes"

export const isAndroid = Platform.OS === "android"
export const isIOS = Platform.OS === "ios"

/**
 *  Takes index of the selected day in the weeek
 *  and returns recurring days of the same index in current month.
 *
 *  For example:
 *   - Sunday (1-08-2021)
 *   - Sunday (8-08-2021)
 *   ...
 *
 *  @param index - the selected index of a week day
 *  @param year
 *  @param month
 *  @returns elected days array in milliseconds
 */
export const getRecurringMonthDays = (
  index: number,
  year: number,
  month: string
) => {
  const daysArray: number[] = []

  const numOfDays = new Date(year, monthsByName[month] + 1, 0).getDate()
  const firstDayOfWeek = new Date(year, monthsByName[month]).getDay()

  var firstDayToSelect =
    firstDayOfWeek > index
      ? 7 - firstDayOfWeek + index + 1
      : 1 + (index - firstDayOfWeek)
  // Calculate number of weeks (+1 because starting from current selected day)
  var numOfWeeks = Math.floor((numOfDays - firstDayToSelect) / 7 + 1)

  for (; numOfWeeks > 0; numOfWeeks--) {
    daysArray.push(
      new Date(year, monthsByName[month], firstDayToSelect).getTime()
    )
    firstDayToSelect = firstDayToSelect + 7
  }

  return daysArray
}

/**
 * Starts challenge sequence to obtain JWT from the server.
 * Returns {id, username, accessToken, expiresIn} or  `null` if failed.
 *
 * @param credential
 * @returns jwt | null
 */
export const startChallengeSequence = async (
  credential: string,
  id: string,
  isSigningUp: boolean
): Promise<{ [index: string]: string } | null> => {
  try {
    let res = await Auth.requestChallenge({ credential, id })
    let { challengeString } = res

    if (challengeString) {
      let signature: any = await signChallenge(
        base64.toByteArray(challengeString)
      )

      if (signature) {
        // request JWT
        let res = await Auth.requestAccessToken(challengeString, signature, {
          id,
          publicKey: credential,
        })
        if (res) return res
      }
    }

    return null
  } catch (e) {
    throw new Error(e.message)
  }
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
  const saltHex = crs({ length: 2 * 32 })
  const nonceHex = crs({ length: 2 * 12 })
  const hexValue = Buffer.from(value).toString("hex")
  const hexPassword = Buffer.from(password).toString("hex")
  return await encrypt_with_password(hexPassword, saltHex, nonceHex, hexValue)
}

//@TODO throw error if password is incorrect
export const decryptWithPassword = async (
  cipherText: string,
  password: string
): Promise<undefined | string> => {
  const hexPassword = Buffer.from(password).toString("hex")
  return await decrypt_with_password(hexPassword, cipherText)
}
