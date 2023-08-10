export class UserProfileDto {
  constructor(
    username: string,
    bio?: string,
    hourlyRateAda?: number,
    profession?: string,
    jobTitle?: string,
    skills?: string
  ) {
    this.bio = bio
    this.profession = profession
    this.skills = skills
    this.hourlyRateAda = hourlyRateAda
    this.jobTitle = jobTitle
    this.username = username
  }
  username: string
  profession?: string
  jobTitle?: string
  skills?: string
  hourlyRateAda?: number
  bio?: string
}
