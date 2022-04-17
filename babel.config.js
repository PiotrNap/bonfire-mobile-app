module.exports = function (api) {
  const presets = ["module:metro-react-native-babel-preset"]
  const plugins = [
    [
      "module:react-native-dotenv",
      {
        moduleName: "@env",
        allowUndefined: false,
        safe: true,
      },
    ],
    [
      require.resolve("babel-plugin-module-resolver"),
      {
        root: ["./src/"],
        alias: {
          // define aliases to shorten the import paths
          assets: "./src/assets",
          common: "./src/common",
          components: "./src/components",
          containers: "./src/containers",
          contexts: "./src/contexts",
          lib: "./src/lib",
          icons: "./src/assets/icons",
          images: "./src/assets/images",
          interfaces: "./src/common/interfaces",
          Api: "./src/services/Api",
          screens: "./src/screens",
          stacks: "./src/stacks",
          styles: "./src/styles",
          tabs: "./src/tabs",
          types: "./src/common/types",
          utils: "./src/lib/utils.ts",
        },
        extensions: [
          ".js",
          ".jsx",
          ".tsx",
          ".ts",
          ".ios.js",
          ".android.js",
          ".jpg",
          ".png",
        ],
      },
    ],
    // reanimated has to be listed as last plugin.
    "react-native-reanimated/plugin",
  ]

  if (api.env("production")) plugins.push("transform-remove-console")

  return {
    presets,
    plugins,
  }
}
