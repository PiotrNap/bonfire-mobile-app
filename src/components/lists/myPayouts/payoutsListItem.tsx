import * as React from "react"
import { View, StyleSheet, Pressable, BackHandler } from "react-native"

import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { getRandomKey } from "lib/utils"
import { EventBookingSlot } from "common/types/dto"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext } from "contexts/contextApi"
import { Checkbox } from "components/forms/Checkbox"
import { lovelaceToAda, schemaToPaymentTokens } from "lib/wallet/utils"
import { fontWeight } from "../../../styles/typography"

export interface PayoutsListItemProps {
  item: EventBookingSlot
  index: number
  isSelected: boolean
  onCheckBoxPress: (idx: number) => void
}

const _PayoutsListItem = React.memo(
  ({ item, index, onCheckBoxPress, isSelected }: PayoutsListItemProps) => {
    const { colorScheme } = appContext()
    const isLightMode = colorScheme === "light"
    const eventCost = React.useCallback(() => schemaToPaymentTokens(item.cost), [])
    const eventCostLovelace = eventCost().lovelace
    const eventCostAssets = eventCost().assets.nTokenTypes

    const onSelected = () => {
      onCheckBoxPress(index)
    }
    const containerBackgrounColor = isLightMode
      ? Colors.primary.s600
      : Colors.primary.s600

    return (
      <Pressable
        key={getRandomKey(2)}
        onPress={onSelected}
        style={Buttons.applyOpacity([
          styles.main,
          { backgroundColor: containerBackgrounColor },
        ])}>
        <View pointerEvents="none" style={styles.textContainer}>
          <SubHeaderText colors={[Colors.primary.neutral]}>
            Event: {item.eventTitle}
          </SubHeaderText>
          <SubHeaderText colors={[Colors.primary.neutral]}>
            {`With: ${item.attendeeAlias}`}
          </SubHeaderText>
          <SubHeaderText colors={[Colors.primary.neutral]}>
            From: {new Date(item.fromDate).toLocaleString()}
          </SubHeaderText>
          <SubHeaderText colors={[Colors.primary.neutral]}>
            To: {new Date(item.toDate).toLocaleString()}
          </SubHeaderText>
          <View style={styles.eventCostWrapper}>
            <SubHeaderText
              customStyle={{ ...fontWeight.semibold }}
              colors={[Colors.primary.s800]}>
              {lovelaceToAda(BigInt(eventCostLovelace))}₳{" "}
            </SubHeaderText>
            {eventCostAssets > 0 && (
              <SubHeaderText
                customStyle={{ ...fontWeight.semibold }}
                colors={[Colors.primary.s800]}>
                + {eventCostAssets} {eventCostAssets === 1 ? "asset" : "assets"}
              </SubHeaderText>
            )}
          </View>
        </View>

        <View pointerEvents="none" style={styles.arrowContainer}>
          <Checkbox
            customContainerStyle={{ marginLeft: "auto" }}
            acceptedCheckbox={isSelected}
            onCheckBoxPress={onSelected}
          />
        </View>
      </Pressable>
    )
  }
)

export const PayoutsListItem = _PayoutsListItem

const styles = StyleSheet.create({
  main: {
    flexDirection: "row",
    alignItems: "center",
    padding: Sizing.x10,
    marginVertical: Sizing.x10,
    ...Outlines.shadow.base,
    borderRadius: Outlines.borderRadius.base,
  },
  arrowContainer: {},
  textContainer: {
    flex: 1,
  },
  eventCostWrapper: {
    flexDirection: "row",
    backgroundColor: Colors.primary.neutral,
    borderRadius: Outlines.borderRadius.base,
    paddingVertical: Sizing.x2,
    paddingHorizontal: Sizing.x5,
    marginRight: "auto",
    marginTop: Sizing.x2,
  },
})
