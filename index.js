import { AppRegistry } from "react-native"
import { name as appName } from "./app.json"
import App from "./App"
import firebase from "@react-native-firebase/app"
import messaging from "@react-native-firebase/messaging"

// 1. Import the firebase module

// 2. Initialize Firebase
if (!firebase.apps.length) {
  firebase.initializeApp({
    // your firebase configuration object
  })
}

// import messaging from "@react-native-firebase/messaging"

AppRegistry.registerComponent(appName, () => App)

// messaging().setBackgroundMessageHandle(async (message) => {
//   console.log("got a new message ", message)
// })
