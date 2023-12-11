import React, { useEffect } from "react"

import { WithSplashScreen } from "./WithSplashScreen"
import App from "./App"

export const AppWithSplashScreen = () => {
  const [isReady, setIsReady] = React.useState(false)

  useEffect(() => {
    async function initialize() {
      await new Promise((resolve) => setTimeout(resolve, 2500))
      setIsReady(true)
    }

    initialize()
  }, [])

  return <WithSplashScreen isAppReady={isReady}>{isReady && <App />}</WithSplashScreen>
}
