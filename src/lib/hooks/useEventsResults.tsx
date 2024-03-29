import { Events } from "Api/Events"
import * as React from "react"

export const useEventsResults = () => {
  const [events, setEvents] = React.useState<any[] | null>(null)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)

  const getEventsBySearchQuery = async (searchValue: string) => {
    try {
      const res: any = await Events.getEventsBySearch(searchValue)
      let data = res.data

      if (data) {
        setEvents(data.result)
      }
    } catch (e) {
      console.error(e)
    } finally {
      setIsLoading(false)
    }
  }

  return {
    isLoading,
    setIsLoading,
    events,
    setEvents,
    getEventsBySearchQuery,
  }
}
