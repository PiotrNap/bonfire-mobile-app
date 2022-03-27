import { LinkingOptions } from "@react-navigation/native"
import { DEEP_LINKING_PATHS } from "common/types/navigationTypes"

/**
 * Return config for deep linking to be used by react-natvigation.
 * This will allow users navigate back to our app from mobile browser.
 */
export const getAthorizedLinkingConfig = (
  profileType: "organizer" | "attendee"
): LinkingOptions => {
  return {
    config: {
      screens: {
        "Onboarding Screens": DEEP_LINKING_PATHS.ONBOARDING,
        "Navigation Screens": {
          path: DEEP_LINKING_PATHS.NAVIGATION,
          screens: {
            Home:
              profileType === "attendee"
                ? DEEP_LINKING_PATHS.HOME
                : {
                    screens: {
                      "Available Days Selection":
                        DEEP_LINKING_PATHS.AVAILABLE_DAYS_SELECTION,
                      "New Event Description":
                        DEEP_LINKING_PATHS.NEW_EVENT_DESCRIPTION,
                    },
                  },
            Browse: {
              screens: {
                "Available Event Days Selection":
                  DEEP_LINKING_PATHS.AVAILABLE_EVENTS_DAYS_SELECTION,
                Browse: DEEP_LINKING_PATHS.BROWSE,
              },
            },
          },
        },
        "Add Funds": DEEP_LINKING_PATHS.ADD_FUNDS,
      },
    },
    prefixes: ["bonfire://gimbalabs.bonfire.com", "bonfire://"],
  }
}

export const getUnauthorizedLinkingConfig = (): LinkingOptions => {
  return {
    config: {
      screens: { "Onboarding Screens": DEEP_LINKING_PATHS.ONBOARDING },
    },
    prefixes: ["bonfire://gimbalabs.bonfire.com", "bonfire://"],
  }
}

export const createNestedPath = (pathsArray: string[]): string => {
  return pathsArray.join("/")
}
