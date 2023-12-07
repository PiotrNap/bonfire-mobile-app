import { TxInput } from "@hyperionbt/helios"

export interface ProfileState {
  username: string
  name: string
  publicKey: string
  id: string
  deviceID: string
  walletBalance: { inputs: TxInput[]; lovelace: number }
  walletName: string
  walletBaseAddress: string
  collateralUtxoId: string
  bio: string
  imageBase64?: string | null
  hasSyncedWallet: boolean
  timeZone?: string
  timeBlockLengthMin: number | null
  timeBlockCostADA: number | undefined
  hourlyRateAda: number
  profession: string | undefined
  jobTitle: string | undefined
  description: string | undefined
  skills: string | undefined
  profile: UserBaseDTO | null
  getUserProfile: () => any
  setName: (input: string) => void
  setUsername: (input: string) => void
  setPublicKey: (input: string) => void
  setID: (input: string) => void
  setDeviceID: (input: string) => void
  setBio: (input: string) => void
  setImageBase64: (input: string) => void
  setTimeZone: (input: string) => void
  setTimeBlockLengthMin: (input: number) => void
  setTimeBlockCostADA: (input: number) => void
  setHourlyRateAda: (inputs: number) => void
  setHasSyncedWallet: (arg: boolean) => void
  setWalletBalance: (balance: { inputs: TxInput[]; lovelace: number }) => void
  setWalletBaseAddress: (input: string) => void
  setWalletName: (input: string) => void
  setProfession: (input: string) => void
  setJobTitle: (input: string) => void
  setDescription: (input: string) => void
  setSkills: (input: string) => void
  setProfile: (input: Partial<UserBaseDTO> | null) => void
  setCollateralUtxoId: (id: string) => void
  resetProfileState: () => void
}

export interface UserBaseDTO {
  username: string
  name: string
  id: string
  deviceID: string
  timeZone?: string
  publicKey?: string
  baseAddress: string
  profession?: string
  jobTitle?: string
  skills?: string
  bio?: string
  hourlyRateAda?: number
  collateralUtxoId: string
}

export interface AddDeviceDTO {
  devicePubKey: string
  walletPublicKey: string
}
