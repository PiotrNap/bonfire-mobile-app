import * as React from "react"
import { View, StyleSheet, Text, Pressable, StyleProp } from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import { LeftArrowIcon, QrCodeIcon } from "icons/index"
import { Typography, Colors, Sizing, Outlines, Buttons } from "styles/index"
import { appContext, walletContext } from "contexts/contextApi"
import { HeaderText } from "components/rnWrappers/headerText"
import { Field, Formik } from "formik"
import { txSendValidationSchema } from "lib/validators"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { CustomInput } from "components/forms/CustomInput"
import { formStyleDark, formStyleLight, inputStyles } from "../../styles/forms"
import { WalletTabList } from "components/wallet/walletTabList"
import { noop } from "lib/utils"
import { showErrorToast } from "lib/helpers"
import { AssetUnit } from "lib/wallet/types"

export const SendTransactionScreen = ({ navigation }: any) => {
  const { walletAssets } = walletContext()
  const { colorScheme } = appContext()
  const { setSendTxInfo, lovelaceBalance } = walletContext()
  const [selectedAssets, setSelectedAssets] = React.useState<Map<string, AssetUnit>>(
    new Map()
  )
  const [isValidForm, setIsValidForm] = React.useState<boolean>(false)
  const [adaAmount, setAdaAmount] = React.useState<string>("")
  const [receiverAddress, setReceiverAddress] = React.useState<string>("")
  const formikRef = React.useRef<any>(null)
  const isLightMode = colorScheme === "light"

  const onBackNavigationPress = () => navigation.goBack()

  const onPreviewPress = () => {
    setSendTxInfo({
      receiverAddress,
      lovelace: Number(adaAmount) * 1_000_000,
      assets: selectedAssets,
    })
    navigation.navigate("Preview Transaction")
  }
  const onScanPress = () => navigation.navigate("Qr-Code Scanner")
  const onAdaAmountChange = (e: any) => setAdaAmount(e.nativeEvent.text)
  const onReceiverAddressChange = (e: any) => setReceiverAddress(e.nativeEvent.text)

  const onCheckboxPress = (unit: string) => {
    const walletAsset = walletAssets?.get(unit)
    if (!walletAsset)
      return showErrorToast("Unable to find selected asset in user wallet")

    const isSelected = selectedAssets.has(unit)
    let newSelectedAssets = selectedAssets

    if (isSelected) {
      const deleted = newSelectedAssets.delete(unit)
      if (!deleted) return

      return setSelectedAssets(newSelectedAssets)
    } else {
      newSelectedAssets.set(unit, walletAsset)
      setSelectedAssets(newSelectedAssets)
    }
  }

  let formStyles: StyleProp<any>
  if (isLightMode) {
    formStyles = Object.assign({}, inputStyles, styles, formStyleLight)
  } else {
    formStyles = Object.assign({}, inputStyles, styles, formStyleDark)
  }

  return (
    <SafeAreaView style={[isLightMode ? styles.safeArea_light : styles.safeArea_dark]}>
      <View style={styles.mainContainer}>
        <View style={styles.navigation}>
          <Pressable onPress={onBackNavigationPress} hitSlop={10}>
            <LeftArrowIcon
              width={24}
              height={24}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>
        <View style={styles.header}>
          <HeaderText colorScheme={colorScheme}>{"Send Funds"}</HeaderText>
        </View>
        <Formik
          validationSchema={txSendValidationSchema(lovelaceBalance)}
          initialValues={{
            receivingAddress: "",
            ada: 2,
          }}
          innerRef={formikRef}
          onSubmit={noop}>
          {({ isValid, validateForm, values }) => {
            React.useEffect(() => {
              if (values.receivingAddress && values.ada) setIsValidForm(isValid)
            }, [isValid, values])
            return (
              <>
                <View style={styles.receiverContainer}>
                  <View style={styles.receiverFieldContainer}>
                    <Field
                      key="receivingAddress"
                      name="receivingAddress"
                      label="Receiver Address"
                      component={CustomInput}
                      keyboardType="default"
                      placeholder={"addr1qqandzg..."}
                      validateForm={validateForm}
                      onChange={onReceiverAddressChange}
                      styles={formStyles}
                      required
                    />
                  </View>
                  <Pressable
                    onPress={onScanPress}
                    style={Buttons.applyOpacity([
                      styles.scanBtn,
                      {
                        backgroundColor: isLightMode
                          ? Colors.primary.s200
                          : Colors.primary.s600,
                        borderColor: isLightMode
                          ? Colors.primary.s200
                          : Colors.primary.s600,
                      },
                    ])}>
                    <QrCodeIcon
                      width={24}
                      height={24}
                      color={isLightMode ? Colors.primary.s800 : "white"}
                      strokeWidth={2}
                    />
                    <Text
                      style={[
                        styles.buttonText,
                        { color: isLightMode ? Colors.primary.s800 : "white" },
                      ]}>
                      Scan
                    </Text>
                  </Pressable>
                </View>
                <Field
                  key="ada"
                  name="ada"
                  label="Ada Amount"
                  component={CustomInput}
                  keyboardType="numeric"
                  validateForm={validateForm}
                  styles={formStyles}
                  onChange={onAdaAmountChange}
                />
              </>
            )
          }}
        </Formik>
        <View style={styles.tabContent}>
          {walletAssets && (
            <WalletTabList
              isSendTransactionScreen
              listData={walletAssets}
              onEndReached={noop}
              selectedAssets={selectedAssets}
              onCheckboxPress={onCheckboxPress}
              type="assets"
            />
          )}
        </View>

        <View>
          <FullWidthButton
            text={"Preview"}
            lightMode={isLightMode}
            disabled={!isValidForm}
            onPressCallback={onPreviewPress}
          />
        </View>
      </View>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea_light: {
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
    flex: 1,
  },
  safeArea_dark: {
    backgroundColor: Colors.neutral.s600,
    alignItems: "center",
    flex: 1,
  },
  mainContainer: {
    flex: 1,
    width: "90%",
    marginBottom: Sizing.x5,
  },
  header: {
    width: "100%",
    marginBottom: Sizing.x5,
  },
  receiverContainer: {
    flexDirection: "row",
    justifyContent: "center",
    alignItems: "center",
  },
  receiverFieldContainer: {
    flex: 1,
  },
  tabContent: {
    flex: 1,
    marginHorizontal: Sizing.x10,
  },
  navigation: {
    flexDirection: "row",
    marginVertical: Sizing.x15,
  },
  buttonText: {
    ...Typography.header.x20,
    color: Colors.primary.s800,
    textAlign: "center",
  },
  scanBtn: {
    alignItems: "center",
    justifyContent: "center",
    borderRadius: 10,
    width: Sizing.x65,
    height: Sizing.x65,
    margin: 5,
  },
  addressWrapper: {
    marginTop: "auto",
    padding: Sizing.x10,
    backgroundColor: Colors.primary.neutral,
    borderWidth: Outlines.borderWidth.base,
    borderRadius: Outlines.borderRadius.base,
    borderColor: Colors.primary.s600,
  },
})
