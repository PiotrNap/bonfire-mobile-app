import axios from "axios"
import { ANDROID_API_URL, IOS_API_URL } from "@env"
import { Platform } from "react-native"

const instance = axios.create({
  baseURL: Platform.OS === "android" ? ANDROID_API_URL : IOS_API_URL,
})

export const setAuthorizationToken = (token: string) => {
  instance.defaults.headers.common["Authorization"] = `Bearer ${token}`
}

instance.defaults.headers.post["Content-Type"] = "application/json"

export default instance
