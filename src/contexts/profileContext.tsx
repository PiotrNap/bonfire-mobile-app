/**
 * @name dataContext
 * @desc Context container for the profile data
 */
import React, { useState, createContext } from "react"
import { ProfileState, UserBaseDTO } from "interfaces/profileInterface"

export const initialState: ProfileState = {
  username: "",
  name: "",
  publicKey: "",
  id: "",
  deviceID: "",
  bio: "",
  imageBase64: "",
  profession: "",
  jobTitle: "",
  description: "",
  skills: "",
  hasSyncedWallet: false,
  timeZone: "",
  timeBlockLengthMin: 0,
  timeBlockCostADA: 0,
  walletBalance: { inputs: [], lovelace: 0 },
  walletName: "",
  walletBaseAddress: "",
  hourlyRateAda: 0,
  profile: null,
  getUserProfile: () => {},
  setName: () => {},
  setUsername: () => {},
  setID: () => {},
  setDeviceID: () => {},
  setPublicKey: () => {},
  setBio: () => {},
  setImageBase64: () => {},
  setTimeZone: () => {},
  setTimeBlockLengthMin: () => {},
  setTimeBlockCostADA: () => {},
  setHourlyRateAda: () => {},
  setHasSyncedWallet: () => {},
  setWalletBalance: () => {},
  setWalletName: () => {},
  setWalletBaseAddress: () => {},
  setProfession: () => {},
  setJobTitle: () => {},
  setDescription: () => {},
  setSkills: () => {},
  setProfile: () => {},
  resetProfileState: () => {},
}

export interface ContextProviderProps {
  children: React.ReactNode
}

export const ProfileContext = createContext<ProfileState>(initialState)

export const ProfileContextProvider = ({ children }: ContextProviderProps) => {
  const [username, setUsername] = useState<string>("")
  const [name, setName] = useState<string>("")
  const [id, setID] = useState<string>("")
  const [deviceID, setDeviceID] = useState<string>("")
  const [publicKey, setPublicKey] = useState<string>("")
  const [bio, setBio] = useState<string>("")
  const [imageBase64, setImageBase64] = useState<string>("")
  const [timeBlockLengthMin, setTimeBlockLengthMin] = useState<number | null>(0)
  const [timeBlockCostADA, setTimeBlockCostADA] = useState<number | undefined>(0)
  const [timeZone, setTimeZone] = useState<string | undefined>("")
  const [hourlyRateAda, setHourlyRateAda] = useState<number>(0)
  const [profession, setProfession] = useState<string | undefined>("")
  const [jobTitle, setJobTitle] = useState<string | undefined>("")
  const [description, setDescription] = useState<string | undefined>("")
  const [skills, setSkills] = useState<string | undefined>("")
  const [hasSyncedWallet, setHasSyncedWallet] = useState<boolean>(false)
  const [walletBalance, setWalletBalance] = useState<any>(initialState.walletBalance)
  const [walletBaseAddress, setWalletBaseAddress] = useState<string>("")
  const [walletName, setWalletName] = useState<string>("")
  const [profile, setProfile] = useState<UserBaseDTO | null>(null)

  const userProfile = {
    username,
    name,
    id,
    deviceID,
    publicKey,
    walletBalance,
    walletName,
    walletBaseAddress,
    bio,
    imageBase64,
    hasSyncedWallet,
    hourlyRateAda,
    timeZone,
    timeBlockLengthMin,
    timeBlockCostADA,
    profession,
    jobTitle,
    description,
    skills,
  }

  const getUserProfile = () => userProfile

  const resetProfileState = () => {
    setUsername("")
    setName("")
    setID("")
    setDeviceID("")
    setPublicKey("")
    setBio("")
    setImageBase64("")
    setTimeBlockLengthMin(0)
    setTimeBlockCostADA(0)
    setTimeZone("")
    setHourlyRateAda(0)
    setProfession("")
    setJobTitle("")
    setDescription("")
    setSkills("")
    setTimeZone("")
    setHasSyncedWallet(false)
    setWalletBalance({ assets: [], lovelace: 0 })
  }

  return (
    <ProfileContext.Provider
      value={{
        ...userProfile,
        getUserProfile,
        setUsername,
        setName,
        setDeviceID,
        setID,
        setPublicKey,
        setBio,
        setImageBase64,
        setWalletBalance,
        setWalletBaseAddress,
        setWalletName,
        setHasSyncedWallet,
        setTimeBlockLengthMin,
        setTimeBlockCostADA,
        setTimeZone,
        setHourlyRateAda,
        setProfession,
        setJobTitle,
        setDescription,
        setSkills,
        setProfile,
        profile,
        resetProfileState,
      }}>
      {children}
    </ProfileContext.Provider>
  )
}
