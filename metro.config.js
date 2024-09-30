const { getDefaultConfig, mergeConfig } = require("@react-native/metro-config")
const blacklist = require("metro-config/src/defaults/exclusionList")

const {
  withSentryConfig
} = require("@sentry/react-native/metro");

/**
 * Metro configuration
 * https://facebook.github.io/metro/docs/configuration
 *
 * @type {import('metro-config').MetroConfig}
 */
const config = {
  resolver: {
    blacklistRE: blacklist([
      /node_modules\/bip39\/src\/wordlists\/(japanese|spanish|italian|french|korean|czech|portuguese|chinese_traditional|chinese_simplified)\.json$/,
    ]),
  },
}

module.exports = withSentryConfig(mergeConfig(getDefaultConfig(__dirname), config))