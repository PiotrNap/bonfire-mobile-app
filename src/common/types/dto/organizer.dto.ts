import { HourlyRate } from "common/interfaces/newEventInterface"

export class OrganizerProfileDto {
  constructor(
    bio: string,
    hourlyRate: HourlyRate,
    profession?: string,
    jobTitle?: string,
    skills?: string
  ) {
    this.bio = bio
    this.profession = profession
    this.skills = skills
    this.hourlyRate.gimbals = hourlyRate.gimbals
    this.hourlyRate.ada = hourlyRate.ada
    this.jobTitle = jobTitle
  }
  profession?: string
  jobTitle?: string
  skills?: string
  hourlyRate: HourlyRate = { ada: 0, gimbals: 0 }
  bio: string
}
