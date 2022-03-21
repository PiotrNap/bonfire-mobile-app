export class OrganizerProfileDto {
  constructor(
    bio: string,
    hourlyRate: number,
    profession?: string,
    jobTitle?: string,
    skills?: string
  ) {
    this.bio = bio;
    this.profession = profession;
    this.skills = skills;
    this.hourlyRate = hourlyRate;
    this.jobTitle = jobTitle;
  }
  profession?: string;
  jobTitle?: string;
  skills?: string;
  hourlyRate: number;
  bio: string;
}
