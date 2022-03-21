import { PaginationRequestDto } from "common/types/dto";
import { CreateEventDto } from "common/types/dto/create-event.dto";
import { EventBookingDto } from "common/types/dto/event-booking.dto";
import axios from "./base";

export class Events {
  public static async createEvent(event: CreateEventDto): Promise<any> {
    try {
      const res = await axios.post("/events", event);
      if (res.data) return res.data;
    } catch (e) {
      if (e.response) throw new Error(e.response.data.message);
    }
  }

  public static async bookEvent(event: EventBookingDto): Promise<any> {
    try {
      const res = await axios.post("events/booking", event);

      if (res.data) return res.data;
    } catch (e) {
      throw new Error(e);
    }
  }

  public static async getEventsByUserId(id: string): Promise<any[] | void> {
    try {
      const res = await axios.get(`/${id}/events`);
      if (res.data) return res.data;
    } catch (e) {
      if (e.response) throw new Error(e.response.data);
    }
  }

  public static async getEventById(id: string): Promise<any | void> {
    try {
      const res = await axios.get(`events/${id}`);
      if (res.data) return res.data;
    } catch (e) {
      if (e.response) throw new Error(e.response.data);
    }
  }

  public static async getAllEvents(
    query: PaginationRequestDto
  ): Promise<any | void> {
    try {
      const res = await axios.get(
        "/events",
        query && {
          params: {
            limit: query.limit,
            page: query.page,
          },
        }
      );
      if (res) {
        return res.data;
      }
    } catch (e) {
      if (e.response) console.error(e.response.data);
    }
  }

  public static async getEventsBySearch(
    searchValue: string
  ): Promise<any[] | void> {
    try {
      return await axios.get(`events/results?search_query=${searchValue}`);
    } catch (e) {
      if (e.response) console.error(e.response.data);
    }
  }

  public static async getGoogleCalendarEvents(query: {
    [key: string]: string | number;
  }): Promise<any[] | void> {
    try {
      const res = await axios.get("auth/google-calendar-events", query);
      if (res) return res.data;
    } catch (e) {
      if (e.response) console.error(e.response.data);
    }
  }
}
