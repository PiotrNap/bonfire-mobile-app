import { User, PaginationRequestDto } from "common/types/dto"
import { CreateEventDto } from "common/types/dto/create-event.dto"
import { EventBookingDto } from "common/types/dto/event-booking.dto"
import { getFormDataFromFilePath } from "lib/helpers"
import { AnyObject } from "yup/lib/types"
import axios from "./base"

export class Events {
  public static async createEvent(event: CreateEventDto): Promise<any> {
    try {
      const res = await axios.post("/events", event)
      if (res.data) return res.data
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async uploadEventImage(filePath: string, eventId: string): Promise<any> {
    try {
      return await axios.post(
        `events/${eventId}/image`,
        getFormDataFromFilePath(filePath),
        {
          headers: {
            "Content-Type": "multipart/form-data",
          },
        }
      )
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async bookEvent(event: EventBookingDto): Promise<any> {
    try {
      const res = await axios.post("events/booking", event)

      if (res.data) return res.data
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async getEventsByUserId(id: string): Promise<any[] | void> {
    try {
      const res = await axios.get(`/${id}/events`)
      if (res.data) return res.data
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async getEventById(id: string): Promise<any | void> {
    try {
      const res = await axios.get(`events/${id}`)
      if (res.data) return res.data
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async getAllEvents(
    query: User & PaginationRequestDto
  ): Promise<any | void> {
    try {
      const res = await axios.get(
        "/events",
        query && {
          params: {
            limit: query.limit,
            page: query.page,
            organizer_id: query.user_id,
          },
        }
      )
      if (res) {
        return res.data
      }
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  /**
   * Query values: search_query, organizer_id
   */
  public static async getEventsBySearch(
    searchValue: string,
    organizer_id?: string
  ): Promise<any[] | void> {
    try {
      return await axios.get("events/results", {
        params: {
          search_query: searchValue,
          organizer_id,
        },
      })
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async getGoogleCalendarEvents(query: {
    [key: string]: string | number
  }): Promise<any[] | void> {
    try {
      const res = await axios.get("auth/google-cal-events", query)
      if (res) return res.data
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async deleteEvent(id: string): Promise<AnyObject | void> {
    try {
      const res = await axios.delete(`events/${id}`)
      if (res) return res.data
    } catch (e) {
      throw e?.response?.data || e
    }
  }
}
