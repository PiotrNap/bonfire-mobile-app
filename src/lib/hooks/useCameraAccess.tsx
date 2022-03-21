import * as React from "react";
import { Alert, Platform } from "react-native";
import {
  check,
  PERMISSIONS,
  RESULTS,
  Rationale,
  request,
  PermissionStatus,
} from "react-native-permissions";

import { launchCamera, CameraOptions } from "react-native-image-picker";

export const useCameraAccess = () => {
  const [access, setAccess] = React.useState<boolean | null>(false);
  const [imageObj, setImgObj] = React.useState<any>(null);
  const os = Platform.OS;

  const checkCameraPermission = async (): Promise<void> => {
    const permission =
      os === "android" ? PERMISSIONS.ANDROID.CAMERA : PERMISSIONS.IOS.CAMERA;
    try {
      const result = await check(permission);
      if (result === RESULTS.GRANTED) {
        setAccess(true);
      } else if (result === RESULTS.BLOCKED) {
        setAccess(false);
      } else {
        setAccess(null);
      }
    } catch (e) {}
  };

  React.useEffect(() => {
    (async () => await checkCameraPermission())();
  }, []);

  const requestCameraAccessAsync = async (): Promise<void> => {
    const rationale: Rationale = {
      title: "Camera permission needed",
      message: "We need access to your camera in order to upload a new photo",
      buttonNegative: "Deny",
      buttonPositive: "Approve",
      buttonNeutral: "Close",
    };

    const permission =
      os === "android" ? PERMISSIONS.ANDROID.CAMERA : PERMISSIONS.IOS.CAMERA;

    try {
      const res: PermissionStatus = await request(permission, rationale);
      if (res !== "granted") {
        Alert.alert(
          "Access needed",
          "We need access to your devices camera for uploading an image.",
          [{ text: "Close", style: "cancel", onPress: () => {} }]
        );
      } else {
        setAccess(true);
      }
    } catch {
      Alert.alert(
        "Something went wrong",
        "Please try taking a picture again. Make sure you have granted access to your devices camera.",
        [{ text: "Close", style: "cancel", onPress: () => {} }]
      );
    }
  };
  const _launchCamera = async () => {
    if (!access) {
      await requestCameraAccessAsync();
    }

    const options: CameraOptions = {
      mediaType: "photo",
      quality: 0.5,
      maxWidth: 768,
      maxHeight: 768,
      saveToPhotos: true,
    };

    launchCamera(options, (res) => {
      if (res.didCancel) return;
      setImgObj(res);
    });
  };
  return {
    access,
    imageObj,
    setImgObj,
    requestCameraAccessAsync,
    launchCamera: _launchCamera,
  };
};
