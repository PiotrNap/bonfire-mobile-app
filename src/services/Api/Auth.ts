import axios from "./base"
export type UserCredential = { id: string; deviceID: string }

export class Auth {
  private static throwCustomError(e: any) {
    throw Error(
      `${e.message} (${String(e.config.method).toUpperCase()} | ${
        e.config.url
      })`
    )
  }
  public static async requestChallenge(
    payload: UserCredential
  ): Promise<any | void> {
    try {
      const res = await axios.post("/auth/challenge", payload)
      if (res.data) return res.data
    } catch (e: any) {
      this.throwCustomError(e)
    }
  }

  public static async requestAccessToken(reqArg: {
    challenge: string
    signature: string
    id: string
    deviceID: string
  }): Promise<{ index: string } | void> {
    try {
      const res = await axios.post(`/auth/login`, reqArg)
      if (res.data) return res.data
    } catch (e) {
      this.throwCustomError(e)
    }
  }

  public static async checkForGoogleAuth(): Promise<any> {
    try {
      const res = await axios.get("/auth/google-oauth-valid")
      return res.data
    } catch (e) {
      this.throwCustomError(e)
    }
  }
}
