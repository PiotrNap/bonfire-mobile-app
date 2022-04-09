import * as React from "react"
import { View, StyleSheet } from "react-native"

import { Layout } from "components/layouts/basicLayout"
import SearchBar from "@pnap/react-native-search-bar"
import { SearchIcon } from "assets/icons"
import { appContext } from "contexts/contextApi"
import { Colors, Outlines, Sizing, Typography, Buttons } from "styles/index"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"

export interface Props {}

export const MyEvents = ({}: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"

  const onSubmitSearch = () => {}
  const onActiveSearch = () => {}
  const onToggleSearchBar = () => {}

  const CustomSearchIcon = React.useCallback(
    () => (
      <SearchIcon
        width={24}
        height={24}
        stroke={
          colorScheme === "light" ? Colors.primary.s800 : Colors.primary.neutral
        }
        strokeWidth={2.4}
        style={{ marginRight: Sizing.x10 }}
      />
    ),
    []
  )
  return (
    <Layout>
      <View style={styles.topContainer}>
        <SearchBar
          onSubmitSearch={onSubmitSearch}
          onActiveSearch={onActiveSearch}
          onToggleSearchBar={onToggleSearchBar}
          customIcon={CustomSearchIcon}
          inputTextStyle={searchStyles.searchBarInput}
          animationDuration={200}
          //@ts-ignore
          buttonStyle={Buttons.applyOpacity(
            Object.assign(
              {},
              searchStyles.searchButton,
              isLightMode
                ? {
                    backgroundColor: Colors.primary.s800,
                  }
                : { backgroundColor: Colors.primary.neutral }
            )
          )}
          buttonTextStyle={Object.assign(
            {},
            isLightMode
              ? { color: Colors.primary.neutral }
              : { color: Colors.primary.s800 },
            searchStyles.searchButtonText
          )}
          underlineActiveColor={
            isLightMode ? Colors.primary.s600 : Colors.primary.s300
          }
          underlineInactiveColor={Colors.neutral.s300}
        />
      </View>
      <View style={styles.main}>
        <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
          Show organizers events here...
        </SubHeaderText>
      </View>
    </Layout>
  )
}

const styles = StyleSheet.create({
  container: { flex: 1 },
  main: {
    flex: 1,
    alignSelf: "center",
    width: "100%",
    height: "100%",
  },
  topContainer: {
    width: "90%",
  },
})

const searchStyles = StyleSheet.create({
  searchToolContainer: {
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "flex-end",
    marginBottom: Sizing.x5,
    marginTop: Sizing.x10,
  },
  searchBarInput: {
    ...Typography.subHeader.x30,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.s600,
    width: "0%",
    borderBottomWidth: Outlines.borderWidth.base,
    paddingVertical: Sizing.x2,
    paddingHorizontal: 0,
  },
  searchButton: {
    borderRadius: Outlines.borderRadius.base,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-between",
    paddingVertical: Sizing.x5,
    paddingHorizontal: Sizing.x10,
    ...Outlines.shadow.base,
  },
  searchButtonText: {
    ...Typography.header.x20,
  },
  searchIcon: {
    marginRight: Sizing.x10,
  },
})
