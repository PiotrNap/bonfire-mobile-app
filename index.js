import { AppRegistry } from "react-native"
import { name as appName } from "./app.json"
import { AppWithSplashScreen } from "./src/AppWithSplashScreen"

AppRegistry.registerComponent(appName, () => AppWithSplashScreen)
