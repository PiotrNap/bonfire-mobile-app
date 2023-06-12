import { AxiosRequestConfig, Method } from "axios"
import { UserBaseDTO } from "common/interfaces/profileInterface"
import { PaginationRequestDto } from "common/types/dto"
import { getFormDataFromFilePath } from "lib/helpers"
import { AnyObject } from "yup/lib/types"
import axios from "./base"

export class Users {
  public static async getUser(id: string): Promise<any> {
    try {
      const res = await axios.get(`users/${id}`)
      const data = res.data
      return data
    } catch (e: any) {
      console.error(e)
      if (e.response) {
        if (e.status === 401) {
          throw new Error("User not authorized")
        } else {
          throw new Error(e.toJSON())
        }
      } else if (e.request) {
        throw new Error("Something went wrong. Please try again later.")
      } else {
        // report problem with creating the request ** e.config **
      }
    }
  }

  public static async getAllOrganizers(
    query?: PaginationRequestDto
  ): Promise<any | void> {
    try {
      const res = await axios.get(
        "/users/organizers",
        query && {
          params: {
            limit: query.limit,
            page: query.page,
          },
        }
      )
      if (res) {
        return res.data
      }
    } catch (e) {
      if (e.response) console.error(e.response.data)
    }
  }

  public static async createAccount(values: any): Promise<UserBaseDTO | void> {
    try {
      const res = await axios.post("/users/register", values)

      if (res) {
        return res.data
      }
    } catch (e: any) {
      throw new Error(e.response.data.message)
    }
  }

  public static async updateUser(values: any, id: string): Promise<any> {
    try {
      const res = await axios.put(`/users/${id}`, values)
      if (res) return res.data
    } catch (e: any) {
      throw new Error(e.response.data.message)
    }
  }

  public static async getUserCalendarEvents(
    id: string,
    currCalendarDate?: Date
  ): Promise<any | void> {
    console.log("args ...", id, currCalendarDate)
    try {
      const res = await axios.get(`/users/${id}/events`, {
        params: { date: currCalendarDate ?? new Date() },
      })

      if (res) return res.data
    } catch (e) {
      throw new Error(e.response.data.message)
    }
  }

  public static async uploadUserImage(filePath: string): Promise<any> {
    try {
      const res = await axios.post(
        "/users/files/profile-image",
        getFormDataFromFilePath(filePath),
        {
          headers: {
            "Content-Type": "multipart/form-data",
          },
        }
      )
      return res
    } catch (e) {
      throw e
    }
  }

  public static async getUserImage(id: string) {
    try {
      const profileImage = await axios.get(`/users/files/profile-image/${id}`)

      return profileImage
    } catch (e) {
      console.error(e.response.data.message)
    }
  }

  public static async deleteUserImage(id: string) {
    try {
      const res = await axios.delete(`/users/files/profile-image/${id}`)
      return res
    } catch (e) {
      console.error(e.response.data.message)
    }
  }

  public static async deleteUserAccount(id: string) {
    try {
      const res = await axios.delete(`/users/${id}`)
      return res
    } catch (e) {
      console.error(e.response.data.message)
    }
  }

  public static async _(
    method: Method,
    url: string,
    data?: AnyObject,
    config?: AxiosRequestConfig
  ) {
    try {
      return await axios[method](url, data || {}, config)
    } catch (e) {
      throw e.response.data
    }
  }
}
