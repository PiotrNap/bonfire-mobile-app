import * as React from "react";
import { Linking } from "react-native";

import { AxiosResponse } from "axios";
import { InAppBrowser } from "react-native-inappbrowser-reborn";

import { Auth } from "Api/Auth";
import { getDeepLinkUri } from "lib/utils";
import axios from "Api/base";

export const useGoogleAuth = () => {
  const [isRequesting, setIsRequesting] = React.useState<boolean>(false);
  const [isValidOauth, setIsValidOauth] = React.useState<boolean>(false);
  const [isLoading, setIsLoading] = React.useState<boolean>(true);

  React.useEffect(() => {
    (async () => {
      try {
        const isValid = await Auth.checkForGoogleAuth();
        if (isValid != null) setIsValidOauth(isValid);
      } catch (e) {}
      setIsLoading(false);
    })();
  }, []);

  const startGoogleAuthentication = async (callbackPath: string) => {
    setIsRequesting(true);
    try {
      const res: AxiosResponse<any> = await axios.get("auth/google-oauth-url", {
        params: { uri: getDeepLinkUri(callbackPath), path: callbackPath },
      });
      const { authUrl } = await res.data;

      // check if device supports this
      const inAppBrowserAvailable = await InAppBrowser.isAvailable();

      if (authUrl && inAppBrowserAvailable) {
        await InAppBrowser.open(authUrl);
      } else Linking.openURL(authUrl);
    } catch (err) {
      throw new Error(err);
    }
    return setIsRequesting(false);
  };

  return {
    isRequesting,
    isValidOauth,
    setIsValidOauth,
    isLoading,
    startGoogleAuthentication,
  };
};
