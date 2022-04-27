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

import { launchCamera, CameraOptions } from "react-native-image-picker"
import { requestAndroidPermission } from "lib/utils"

export const useCameraAccess = () => {
  const [access, setAccess] = React.useState<boolean | null>(false)
  const [imageObj, setImgObj] = React.useState<any>(null)
  const os = Platform.OS

  const checkCameraPermission = async (): Promise<void> => {
    const permission =
      os === "android" ? PERMISSIONS.ANDROID.CAMERA : PERMISSIONS.IOS.CAMERA
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
    ;(async () => await checkCameraPermission())()
  }, [])

  const requestCameraAccessAsync = async (): Promise<boolean> => {
    const rationale: Rationale = {
      title: "Camera permission needed",
      message: "We need access to your camera in order to upload a new image.",
      buttonNegative: "Deny",
      buttonPositive: "Approve",
      buttonNeutral: "Close",
    }

    const permission =
      os === "android" ? PERMISSIONS.ANDROID.CAMERA : PERMISSIONS.IOS.CAMERA

    try {
      const res: PermissionStatus = await request(permission, rationale)
      if (res !== "granted") {
        Alert.alert(
          "Access needed",
          "We need access to your devices camera for uploading images.",
          [
            { text: "Close", style: "cancel", onPress: () => {} },
            {
              text: "Proceed",
              style: "default",
              onPress: async () =>
                await requestAndroidPermission(
                  "android.permission.CAMERA",
                  "uploading new images."
                ),
            },
          ]
        )
        return false
      } else {
        return true
      }
    } catch {
      Alert.alert(
        "Something went wrong",
        "Please try taking a picture again. Make sure you have granted access to your devices camera.",
        [{ text: "Close", style: "cancel", onPress: () => {} }]
      )
      return false
    }
  }
  const _launchCamera = async (modalRef: any) => {
    if (!access) {
      const _access = await requestCameraAccessAsync()
      if (!_access) return
      setAccess(true)
    }

    const options: CameraOptions = {
      mediaType: "photo",
      quality: 0.5,
      maxWidth: 768,
      maxHeight: 768,
      saveToPhotos: true,
      includeBase64: true,
    }

    launchCamera(options, (res) => {
      if (res.didCancel) setImgObj(res)
      modalRef?.close()
    })
  }
  return {
    access,
    imageObj,
    setImgObj,
    requestCameraAccessAsync,
    launchCamera: _launchCamera,
  }
}
