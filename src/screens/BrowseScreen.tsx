import * as React from "react"
import { View, StyleSheet, Animated } from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import { StackScreenProps } from "@react-navigation/stack"
import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { BookingStackParamList } from "common/types/navigationTypes"
import { appContext } from "contexts/contextApi"
import { EventsList } from "components/booking/EventsList"
import { applyOpacity } from "../styles/colors"
import { useEventsResults } from "lib/hooks/useEventsResults"
import { SearchIcon } from "assets/icons"
import SearchBar from "@pnap/react-native-search-bar"
import { ProfileContext } from "contexts/profileContext"

export interface BrowseProps
  extends StackScreenProps<BookingStackParamList, "Browse"> {
  children: React.ReactNode
}

export const BrowseScreen = ({}: BrowseProps) => {
  const { id } = React.useContext(ProfileContext)
  const { colorScheme } = appContext()
  const {
    events: searchEvents,
    isLoading,
    getEventsBySearchQuery,
    setEvents,
  } = useEventsResults(id)

  const animatedOpacity = React.useRef(new Animated.Value(0)).current
  const isLightMode = colorScheme !== "dark"

  const onActiveSearch = (active: boolean) => {
    Animated.timing(animatedOpacity, {
      useNativeDriver: true,
      toValue: active ? 1 : 0,
      duration: 140,
    }).start()
  }
  const onSubmitSearch = (val: string) => {
    onActiveSearch(false)
    getEventsBySearchQuery(val)
  }
  const onToggleSearchBar = (val: boolean) => {
    // user hides search bar? show the normal events list.
    if (!val) setEvents(null)
  }

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
    [colorScheme]
  )

  /**
   * Old code for displaying horizontal lists (categories, organizers, etc.)
   */
  // const renderFeaturedLists = React.useCallback(() => {
  //   return browseFeatured.map((list, index) => (
  // <HorizontalCardsList navigateTo={navigateTo} key={index} list={list} />
  //   ));
  // }, [browseFeatured]);

  // const navigateTo = (params: BookingStackParamList["Available Dates"]) => {
  //   navigation.navigate("Available Dates", params);
  // };

  return (
    <SafeAreaView
      style={[isLightMode ? styles.safeArea_light : styles.safeaArea_dark]}>
      <View style={styles.topContainer}>
        <SearchBar
          onSubmitSearch={onSubmitSearch}
          onActiveSearch={onActiveSearch}
          onToggleSearchBar={onToggleSearchBar}
          customIcon={CustomSearchIcon}
          inputTextStyle={Object.assign({}, searchStyles.searchBarInput, {
            color: isLightMode ? Colors.primary.s600 : Colors.primary.neutral,
          })}
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
          underlineActiveColor={Colors.primary.s600}
          underlineInactiveColor={Colors.neutral.s300}
        />
      </View>
      <View style={styles.main}>
        <EventsList customIsLoading={isLoading} customEvents={searchEvents} />
        <Animated.View
          pointerEvents="none"
          style={[
            styles.overlay,
            {
              opacity: animatedOpacity,
              zIndex: 20,
            },
          ]}
        />
      </View>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea_light: {
    flex: 1,
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
  },
  safeaArea_dark: {
    flex: 1,
    backgroundColor: Colors.neutral.s600,
    alignItems: "center",
  },
  topContainer: {
    width: "90%",
  },
  main: {
    flex: 1,
    width: "100%",
  },
  overlay: {
    position: "absolute",
    top: 0,
    width: "100%",
    height: "100%",
    backgroundColor: applyOpacity(Colors.neutral.s500, 0.5),
  },
  noEventsMessage: {
    flex: 1,
    alignItems: "center",
    justifyContent: "center",
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
