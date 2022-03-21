import axios from "./base";

export class ChallengeResponseDTO {
  constructor(challenge: string, signature: string, userCredential: any) {
    this.challengeString = challenge;
    this.signature = signature;
    this.userCredential = userCredential;
  }

  challengeString: string;
  signature: string;
  // this value is used for looking up user in db
  userCredential: UserCredential;
}

export type UserCredential = { [index: string]: any };

export class Auth {
  public static async requestChallenge(
    credential: UserCredential
  ): Promise<any | void> {
    try {
      const res = await axios.post("/auth/challenge", credential);
      if (res.data) return res.data;
    } catch (e) {
      if (e.response) console.error(e.response.data);
    }
  }

  public static async requestAccessToken(
    challenge: string,
    signature: string,
    userCredential: UserCredential
  ): Promise<{ index: string } | void> {
    const challengeRequestDTO = new ChallengeResponseDTO(
      challenge,
      signature,
      userCredential
    );

    try {
      const res = await axios.post(`/auth/login`, challengeRequestDTO);
      if (res.data) return res.data;
    } catch (e) {
      if (e.response) console.error(e.response.data);
    }
  }

  public static async checkForGoogleAuth(): Promise<any> {
    try {
      const res = await axios.get("/auth/google-oauth-valid");
      return res.data;
    } catch (e) {
      console.error(e.response);
    }
  }
}
