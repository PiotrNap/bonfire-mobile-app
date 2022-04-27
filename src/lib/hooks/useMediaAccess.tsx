import * as React from "react"
import { Alert, Platform } from "react-native"
import {
  check,
  PERMISSIONS,
  RESULTS,
  Rationale,
  request,
  PermissionStatus,
} from "react-native-permissions"

import {
  launchImageLibrary,
  ImageLibraryOptions,
} from "react-native-image-picker"

export const useMediaAccess = () => {
  const [access, setAccess] = React.useState<boolean | null>(false)
  const [mediaObj, setMediaObj] = React.useState<any>(null)
  const os = Platform.OS

  const checkImageLibraryPermission = async (): Promise<void> => {
    const permission =
      os === "android"
        ? PERMISSIONS.ANDROID.READ_EXTERNAL_STORAGE
        : PERMISSIONS.IOS.PHOTO_LIBRARY
    try {
      const result = await check(permission)
      if (result === RESULTS.GRANTED) {
        setAccess(true)
      } else if (result === RESULTS.BLOCKED) {
        setAccess(false)
      } else {
        setAccess(null)
      }
    } catch (e) {}
  }

  React.useEffect(() => {
    ;(async () => await checkImageLibraryPermission())()
  }, [])

  const requestImageLibraryAccessAsync = async (): Promise<boolean> => {
    const rationale: Rationale = {
      title: "Media library permission needed",
      message:
        "We need access to your media library in order to upload a new image.",
      buttonNegative: "Deny",
      buttonPositive: "Approve",
      buttonNeutral: "Close",
    }

    const permission =
      os === "android"
        ? PERMISSIONS.ANDROID.READ_EXTERNAL_STORAGE
        : PERMISSIONS.IOS.PHOTO_LIBRARY

    try {
      const res: PermissionStatus = await request(permission, rationale)
      if (res !== "granted") {
        Alert.alert(
          "Access needed",
          "We need access to your media library for uploading images.",
          [{ text: "Close", style: "cancel", onPress: () => {} }]
        )
        return false
      } else {
        return true
      }
    } catch {
      Alert.alert(
        "Something went wrong",
        "Please try accessing media library again. Make sure you have granted the access.",
        [{ text: "Close", style: "cancel", onPress: () => {} }]
      )
      return false
    }
  }
  const _launchImageLibrary = async (modalRef: any) => {
    if (!access) {
      const _access = await requestImageLibraryAccessAsync()
      if (!_access) return
    }

    const options: ImageLibraryOptions = {
      mediaType: "photo",
      maxWidth: 768,
      maxHeight: 768,
      quality: 0.5,
      selectionLimit: 1,
      includeBase64: true,
    }

    launchImageLibrary(options, (res) => {
      if (!res.didCancel) setMediaObj(res)
      modalRef?.close()
    })
  }

  return {
    access,
    mediaObj,
    setMediaObj,
    requestImageLibraryAccessAsync,
    launchImageLibrary: _launchImageLibrary,
  }
}
