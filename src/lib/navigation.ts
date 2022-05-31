import { LinkingOptions } from "@react-navigation/native"
import { DEEP_LINKING_PATHS } from "common/types/navigationTypes"

const APP_PREFIXES = ["bonfire://bonfire.com", "bonfire://"]

/**
 * Return config for deep linking to be used by react-navigation.
 * This will allow users navigate back to our app from mobile browser.
 *
 * config screen keys are NAMES of screens, values can be custom paths
 */
export const getAuthorizedLinkingConfig = (
  profileType: "organizer" | "attendee"
): LinkingOptions => ({
  config: {
    screens: {
      "Onboarding Screens": DEEP_LINKING_PATHS.ONBOARDING,
      ...(profileType === "organizer"
        ? {
            "Organizer Navigation Screens": {
              path: DEEP_LINKING_PATHS.NAVIGATION,
              screens: {
                HomeStack: {
                  path: "home",
                  screens: {
                    "Available Days Selection":
                      DEEP_LINKING_PATHS.AVAILABLE_DAYS_SELECTION,
                    "New Event Description":
                      DEEP_LINKING_PATHS.NEW_EVENT_DESCRIPTION,
                  },
                },
                "Browse Stack": {
                  screens: {
                    Browse: DEEP_LINKING_PATHS.BROWSE,
                  },
                },
              },
            },
          }
        : {
            "Attendee Navigation Screens": {
              path: DEEP_LINKING_PATHS.NAVIGATION,
              screens: {
                "Browse Stack": {
                  screens: {
                    Browse: DEEP_LINKING_PATHS.BROWSE,
                  },
                },
              },
            },
          }),
      "Add Funds": DEEP_LINKING_PATHS.ADD_FUNDS,
    },
  },
  prefixes: APP_PREFIXES,
})

export const getUnauthorizedLinkingConfig = (): LinkingOptions => ({
  config: {
    screens: { "Onboarding Screens": DEEP_LINKING_PATHS.ONBOARDING },
  },
  prefixes: APP_PREFIXES,
})

export const createNestedPath = (pathsArray: string[]): string => {
  return pathsArray.join("/")
}
