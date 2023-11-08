import axios from "./base"
export type UserCredential = { id: string; deviceID: string }
export type JWTRequestPayload = {
  challenge: string
  signature: string
  id: string
  deviceID: string
}

export class Auth {
  public static async requestChallenge(payload: UserCredential): Promise<any | void> {
    try {
      const res = await axios.post("/auth/challenge", payload)
      if (res.data) return res.data
    } catch (e: any) {
      throw e?.response?.data || e
    }
  }

  public static async requestAccessToken(
    reqArg: JWTRequestPayload
  ): Promise<{ index: string } | void> {
    console.log("req obj: ", reqArg)
    try {
      const res = await axios.post(`/auth/login`, reqArg)
      if (res.data) return res.data
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async checkForGoogleAuth(): Promise<any> {
    try {
      const res = await axios.get("/auth/google-oauth-valid")
      return res.data
    } catch (e) {
      throw e?.response?.data || e
    }
  }
}
