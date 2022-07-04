import { AppRegistry } from "react-native"
import App from "./App"
import { name as appName } from "./app.json"

// import messaging from "@react-native-firebase/messaging"

AppRegistry.registerComponent(appName, () => App)

// messaging().setBackgroundMessageHandle(async (message) => {
//   console.log("got a new message ", message)
// })
