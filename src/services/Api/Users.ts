import { AxiosRequestConfig, Method } from "axios"
import { AddDeviceDTO, UserBaseDTO } from "common/interfaces/profileInterface"
import { PaginationRequestDto } from "common/types/dto"
import { getFormDataFromFilePath } from "lib/helpers"
import { AnyObject } from "yup/lib/types"
import axios from "./base"

type UserByPublickey = {
  username: string
  baseAddress: string
}

export class Users {
  public static async getUser(id: string): Promise<any> {
    try {
      const res = await axios.get(`users/${id}`)
      const data = res.data
      return data
    } catch (e: any) {
      throw e?.response?.data || e
    }
  }
  public static async getUserByPublickey(baseAddress: string): Promise<any> {
    try {
      const res = await axios.get(`users/base-address/${baseAddress}`)
      const data = res.data
      return data
    } catch (e: any) {
      console.error(e)
      throw e?.response?.data || e
    }
  }
  public static async checkIfUserExistsForPublicKey(
    publickey: string
  ): Promise<UserByPublickey | void> {
    try {
      const res = await axios.get(`users/check-publickey?publickey=${publickey}`)
      const data = res.data
      return data
    } catch (e: any) {
      console.error(e)
      throw e?.response?.data || e
    }
  }
  public static async checkUsernameAvailability(username: string) {
    try {
      const res = await axios.get(`users/check-username?username=${username}`)
      const data = res.data
      return data
    } catch (e: any) {
      console.error(e)
      throw e?.response?.data || e
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
      throw e?.response?.data || e
    }
  }

  public static async registerDevice(values: AddDeviceDTO): Promise<UserBaseDTO | void> {
    try {
      const res = await axios.post("/users/register-device", values)

      if (res) {
        return res.data
      }
    } catch (e: any) {
      throw e?.response?.data || e
    }
  }

  public static async createAccount(values: any): Promise<UserBaseDTO | void> {
    try {
      const res = await axios.post("/users/register", values)

      if (res) {
        return res.data
      }
    } catch (e: any) {
      throw e?.response?.data || e
    }
  }

  public static async updateUser(values: any, id: string): Promise<any> {
    try {
      const res = await axios.put(`/users/${id}`, values)
      if (res) return res.data
    } catch (e: any) {
      throw e?.response?.data || e
    }
  }

  public static async getUserCalendarEvents(
    id: string,
    currCalendarDate?: Date
  ): Promise<any | void> {
    try {
      const res = await axios.get(`/users/${id}/events`, {
        params: { date: currCalendarDate ?? new Date() },
      })

      if (res) return res.data
    } catch (e) {
      throw e?.response?.data || e
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
      throw e?.response?.data || e
    }
  }

  public static async getUserImage(id: string) {
    try {
      const profileImage = await axios.get(`/users/files/profile-image/${id}`)

      return profileImage
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async deleteUserImage(id: string) {
    try {
      const res = await axios.delete(`/users/files/profile-image/${id}`)
      return res
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async deleteUserAccount(id: string) {
    try {
      const res = await axios.delete(`/users/${id}`)
      return res
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async getIsBetaTestingActive() {
    try {
      return await axios.get("/users/beta-tokens/")
    } catch (e) {
      throw e?.response?.data || e
    }
  }

  public static async registerForBetaTesting(betaCode: string, baseAddress: string) {
    try {
      return await axios.get(`/users/beta-tokens/${betaCode}/${baseAddress}`)
    } catch (e) {
      throw e?.response?.data || e
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
      throw e?.response?.data || e
    }
  }
}
