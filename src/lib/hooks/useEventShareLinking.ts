import * as R from "react"
import { Linking } from "react-native"
import { Events } from "Api/Events"
import { bufferToBase64 } from "lib/utils"
import { isUUID } from "lib/helpers"

export const useEventShareLinking = (navigation: any) => {
  const eventListener = async (event: { url: string }) => {
    const eventId = event.url.split("/").reverse()[0]
    if (isUUID(eventId)) await navigateToEvent(eventId)
  }
  const navigateToEvent = async (_id: string) => {
    if (_id)
      try {
        const event = await Events.getEventById(_id)
        const navigationParams = {
          fromShareLink: true,
          title: event.title,
          description: event.description,
          fromDate: event.fromDate,
          toDate: event.toDate,
          image:
            event.eventCardImage?.data &&
            bufferToBase64(event.eventCardImage.data),
          eventId: event.id,
          organizerId: event.organizerId,
          organizerAlias: event.organizerAlias,
          color: event.eventCardColor,
          titleColor: event.eventTitleColor,
          ...event,
        }

        navigation.navigate("Event Description", navigationParams)
      } catch (e) {
        console.error(e.message)
      }
  }

  R.useEffect(() => {
    const subscription = Linking.addEventListener("url", eventListener)
    return () => subscription.remove()
  }, [])

  return { navigateToEvent }
}
