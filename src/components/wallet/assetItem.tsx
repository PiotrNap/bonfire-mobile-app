import * as React from "react"
import { View, Text, StyleSheet, Image } from "react-native"

// FastImage seem not to work with static resources (?)
import FastImage from "react-native-fast-image"
import { Typography, Colors, Sizing, Outlines } from "styles/index"
import {} from "@hyperionbt/helios"

//@ts-ignore
import { appContext } from "contexts/contextApi"
import { AssetUnit } from "lib/wallet/types"
import { Wallet } from "lib/wallet"
import { ipfsToHttp, toLabel } from "lib/wallet/utils"
import { Toast } from "react-native-toast-message/lib/src/Toast"

export interface assetItemInterface {
  item: AssetUnit
}

export const AssetItem = React.memo(({ item }: assetItemInterface) => {
  const [imageUrl, setImageUrl] = React.useState<any>("")
  const { policyId, name, labelNum, count } = item
  const { colorScheme } = appContext()
  const assetName = Buffer.from(name, "hex").toString()

  const getAssetInfo = React.useCallback(async () => {
    const label = labelNum ? toLabel(labelNum) : ""
    const unit = policyId + (label ? label : "") + name
    const { error, data } = await Wallet.getAssetInfo(unit)

    if (error) {
      Toast.show({
        type: "error",
        text1: error?.error || "Something went wrong. Try again?",
        text2: error?.message,
      })
    }
    const ipfsUrl = data?.onchain_metadata?.image || data?.onchain_metadata?.files?.[0]
    if (!ipfsUrl) return
    const ipfsHttpUrl = ipfsToHttp(ipfsUrl)
    setImageUrl(ipfsHttpUrl)
  }, [item])

  React.useEffect(() => {
    item && getAssetInfo()
  }, [item])

  return (
    <View style={styles.container}>
      {imageUrl ? (
        <FastImage
          source={{
            uri: imageUrl,
            priority: FastImage.priority.high,
          }}
          style={styles.assetPic}
        />
      ) : (
        <View style={styles.fallbackImageContainer}>
          <Text style={styles.fallbackImageText}>{assetName.charAt(0)}</Text>
        </View>
      )}
      <View style={styles.assetDetails}>
        <Text
          style={colorScheme === "light" ? styles.userName_light : styles.userName_dark}>
          {assetName}
        </Text>
        <Text
          style={
            colorScheme === "light" ? styles.assetInfo_light : styles.assetInfo_dark
          }>
          #{policyId.substring(0, 8)}...
          {policyId.substring(policyId.length - 8, policyId.length)}
        </Text>
      </View>
      <Text
        style={[
          styles.amount,
          { color: colorScheme === "light" ? Colors.neutral.s800 : Colors.neutral.s100 },
        ]}>
        {count}
      </Text>
    </View>
  )
})

const styles = StyleSheet.create({
  container: {
    flexDirection: "row",
    alignItems: "center",
    marginBottom: Sizing.x25,
    height: Sizing.x55,
  },
  assetPic: {
    borderRadius: Outlines.borderRadius.max,
    width: Sizing.x55,
    height: Sizing.x55,
  },
  assetDetails: {
    marginLeft: Sizing.x15,
  },
  userName_light: {
    ...Typography.header.x30,
    color: Colors.primary.s800,
  },
  userName_dark: {
    ...Typography.header.x30,
    color: Colors.primary.neutral,
  },
  assetInfo_light: {
    ...Typography.subHeader.x20,
    color: Colors.primary.s600,
  },
  assetInfo_dark: {
    ...Typography.subHeader.x20,
    color: Colors.primary.s180,
  },
  amount: {
    marginLeft: "auto",
    ...Typography.header.x35,
  },
  fallbackImageContainer: {
    width: Sizing.x55,
    height: Sizing.x55,
    backgroundColor: "#ddd",
    justifyContent: "center",
    alignItems: "center",
    borderRadius: Outlines.borderRadius.max,
  },
  fallbackImageText: {
    ...Typography.subHeader.x25,
    color: "#333",
  },
})
