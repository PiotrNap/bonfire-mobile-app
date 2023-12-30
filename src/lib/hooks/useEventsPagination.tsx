import { Events } from "Api/Events"
import { PAGINATED_RESULTS_COUNT } from "common/types/dto"
import { appContext } from "contexts/contextApi"
import * as React from "react"

export const useEventsPagination = (id: string, reload = false) => {
  const { networkId } = appContext()
  const [events, setEvents] = React.useState<any[]>([])
  const [eventsPage, setEventsPage] = React.useState<number>(0) // the current page
  const [isLoading, setIsLoading] = React.useState<boolean>(true)
  const [isLastPage, setIsLastPage] = React.useState<boolean>(false)

  const getEventsPaginated = async (page?: number, limit = 10, isRefreshing = false) => {
    if ((isLastPage && page !== 1) || (page === eventsPage && !isRefreshing)) return
    setIsLoading(true)
    try {
      const res = await Events.getAllEvents({
        limit,
        ...(id ? { user_id: id } : {}),
        page: page ?? eventsPage,
        network_id: networkId,
      })

      // no more events left in DB
      if (res.count < limit) setIsLastPage(true)

      if (res) {
        setEvents((prev) => [...(!isRefreshing ? prev : []), ...res.result])
        setEventsPage(page ?? 1)
      }
    } catch (e) {
      console.error(e)
    } finally {
      setIsLoading(false)
    }
  }

  React.useEffect(() => {
    if (id) getEventsPaginated(1, PAGINATED_RESULTS_COUNT, true)
  }, [id, reload, networkId])

  return {
    isLoading,
    isLastPage,
    events,
    eventsPage,
    setEventsPage,
    getEventsPaginated,
  }
}
