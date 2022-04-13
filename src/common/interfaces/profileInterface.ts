export interface ProfileState {
  username: string
  name: string
  publicKey: string
  id: string
  walletBalance: number
  bio: string
  imageBase64?: string | null
  hasSyncedWallet: boolean
  timeBlockLengthMin: number | null
  timeBlockCostADA: number | undefined
  hourlyRate: number
  profession: string | undefined
  jobTitle: string | undefined
  description: string | undefined
  skills: string | undefined
  profileType: "attendee" | "organizer" | ""
  getUserProfile: () => any
  setName: (input: string) => void
  setUsername: (input: string) => void
  setPublicKey: (input: string) => void
  setId: (input: string) => void
  setBio: (input: string) => void
  setImageBase64: (input: string) => void
  setTimeBlockLengthMin: (input: number) => void
  setTimeBlockCostADA: (input: number) => void
  setHourlyRate: (input: number) => void
  setHasSyncedWallet: (arg: boolean) => void
  setWalletBalance: (input: number) => void
  setProfession: (input: string) => void
  setJobTitle: (input: string) => void
  setDescription: (input: string) => void
  setSkills: (input: string) => void
  setProfileType: (input: "organizer" | "attendee" | "") => void
  resetProfileState: () => void
}

export interface UserBaseDTO {
  username: string
  name: string
  id: string
  publicKey?: string
}

export interface UserOrganizerDTO {
  profession: string
  jobTitle: string
  skills: string
  bio: string
  hourlyRate: number
}
