import React, { useCallback } from "react"
import { Pressable, StyleSheet, View } from "react-native"

import {
  Camera,
  useCameraDevice,
  useCodeScanner,
  useCameraPermission,
} from "react-native-vision-camera"
import { Toast } from "react-native-toast-message/lib/src/Toast"
import { Crypto } from "@hyperionbt/helios"
import { appContext } from "contexts/contextApi"
import { Buttons, Colors, Outlines, Sizing } from "styles/"
import { LeftArrowIcon } from "assets/icons"
import { SafeAreaView, useSafeAreaFrame } from "react-native-safe-area-context"

export function QrCodeScannerScreen({ navigation, route }: any) {
  const { setQrCodeValue } = appContext()
  const [isCameraActive, setIsCameraActive] = React.useState<boolean>(false)
  const cameraPermission = useCameraPermission()

  const navigateBack = () => navigation.goBack()
  const device = useCameraDevice("back")
  const codeScanner = useCodeScanner({
    codeTypes: ["qr", "ean-13"], // specify the types of codes you want to scan
    onCodeScanned: (codes) => {
      let code = codes[0]?.value ?? ""

      const correctAddress = Crypto.verifyBech32(code)

      if (!code || !correctAddress)
        return Toast.show({
          type: "error",
          text1: "Scan Error",
          text2: "The code you are trying to scan is incorrect or malformed.",
        })

      setQrCodeValue(code)
      setIsCameraActive(false)
      navigateBack()
    },
  })

  React.useEffect(() => {
    if (!isCameraActive) setIsCameraActive(true)
    if (!device) navigation.goBack()

    if (device && !cameraPermission.hasPermission) {
      ;(async () => cameraPermission.requestPermission())()
    }
  }, [])

  return device ? (
    <SafeAreaView style={styles.container}>
      <Pressable
        style={Buttons.applyOpacity(styles.navigation)}
        onPress={navigateBack}
        hitSlop={10}>
        <LeftArrowIcon width={24} height={24} color={Colors.primary.s600} />
      </Pressable>
      <Camera
        style={StyleSheet.absoluteFillObject}
        isActive={isCameraActive}
        device={device}
        codeScanner={codeScanner}
      />
    </SafeAreaView>
  ) : (
    <></>
  )
}

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
  },
  container: {
    flex: 1,
  },
  camera: {
    ...StyleSheet.absoluteFillObject,
  },
  overlay: {
    ...StyleSheet.absoluteFillObject,
    position: "absolute",
  },
  overlayRow: {
    flexDirection: "row",
    flex: 1,
  },
  overlaySection: {
    backgroundColor: "rgba(0, 0, 0, 0.5)",
  },
  navigation: {
    position: "absolute",
    left: Sizing.x12,
    top: Sizing.x10,
    zIndex: 99,
    alignItems: "center",
    justifyContent: "center",
    width: Sizing.x45,
    height: Sizing.x45,
    backgroundColor: Colors.primary.neutral,
    borderRadius: Outlines.borderRadius.max,
  },
  overlayHole: {
    backgroundColor: "transparent",
    borderColor: "white",
    borderWidth: 2,
    borderStyle: "dashed",
  },
})
