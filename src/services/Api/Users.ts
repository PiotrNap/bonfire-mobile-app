import { UserDTO } from "common/interfaces/profileInterface";
import { PaginationRequestDto } from "common/types/dto";
import { Platform } from "react-native";
import axios from "./base";

export class Users {
  public static async getUser(id: string): Promise<any> {
    try {
      const res = await axios.get(`user/${id}`);
      const data = res.data;
      return data;
    } catch (e: any) {
      if (e.response) {
        if (e.status === 401) {
          throw new Error("User not authorized");
        } else {
          throw new Error(e.toJSON());
        }
      } else if (e.request) {
        throw new Error("Something went wrong. Please try again later.");
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
      );
      if (res) {
        return res.data;
      }
    } catch (e) {
      if (e.response) console.error(e.response.data);
    }
  }

  public static async createAccount(values: any): Promise<UserDTO | void> {
    try {
      const res = await axios.post("/users/register", values);

      if (res) {
        return res.data;
      }
    } catch (e: any) {
      throw new Error(e.response.data.message);
    }
  }

  public static async updateUser(values: any, id: string): Promise<any> {
    try {
      const res = await axios.put(`/users/${id}`, values);
      if (res) return res.data;
    } catch (e: any) {
      if (e.response) console.error(e.response.data);
    }
  }

  public static async getUserCalendarEvents(
    id: string,
    currCalendarDate?: Date
  ): Promise<any | void> {
    console.log("args ...", id, currCalendarDate);
    try {
      const res = await axios.get(`/users/${id}/calendar-events`, {
        params: { date: currCalendarDate ?? new Date() },
      });
      console.log("res?", !!res);

      if (res) return res.data;
    } catch (e) {
      throw new Error(e.response.data.message);
    }
  }

  public static async uploadUserImage(filePath: string) {
    const fileChunks = filePath.split(".");
    const fileType = fileChunks[fileChunks.length - 1];
    const formData = new FormData();

    console.log("file type :", fileType, fileChunks);

    formData.append("file", {
      uri: Platform.OS === "ios" ? filePath.replace("file://", "") : filePath,
      name: `photo.${fileType}`,
      type: `image/${fileType}`,
    });

    try {
      // console.log(JSON.stringify(axios.defaults, null, 4));
      const res = await axios.post("/users/files/profile-image", formData, {
        headers: {
          "Content-Type": "multipart/form-data",
        },
      });
      console.log("res :", res);
    } catch (e) {
      console.error("Error: ", e);
      console.error("Error Message: ", e.response.data.message);
    }
  }

  public static async getUserImage(id: string) {
    try {
      const profileImage = await axios.get(`/users/files/profile-image/${id}`);
      console.log(profileImage);

      return;
    } catch (e) {
      console.error(e.response.data.message);
    }
  }
}
