/**
 * @name dataContext
 * @desc Context container for the profile data
 */
import React, { useState, createContext } from "react"
import { ProfileState } from "interfaces/profileInterface"
import { HourlyRate } from "common/interfaces/newEventInterface"

export const initialState: ProfileState = {
  username: "",
  name: "",
  publicKey: "",
  id: "",
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
  walletBalance: 0,
  profileType: "",
  timeZone: "",
  hourlyRate: { ada: 0, gimbals: 0 },
  getUserProfile: () => {},
  setName: () => {},
  setUsername: () => {},
  setProfileType: () => {},
  setId: () => {},
  setPublicKey: () => {},
  setBio: () => {},
  setImageBase64: () => {},
  setTimeZone: () => {},
  setTimeBlockLengthMin: () => {},
  setTimeBlockCostADA: () => {},
  setTimeZone: () => {},
  setHourlyRate: () => {},
  setHasSyncedWallet: () => {},
  setWalletBalance: () => {},
  setProfession: () => {},
  setJobTitle: () => {},
  setDescription: () => {},
  setSkills: () => {},
  resetProfileState: () => {},
}

export interface ContextProviderProps {
  children: React.ReactNode
}

export const ProfileContext = createContext<ProfileState>(initialState)

export const ProfileContextProvider = ({ children }: ContextProviderProps) => {
  const [username, setUsername] = useState<string>("")
  const [name, setName] = useState<string>("")
  const [id, setId] = useState<string>("")
  const [publicKey, setPublicKey] = useState<string>("")
  const [bio, setBio] = useState<string>("")
  const [imageBase64, setImageBase64] = useState<string>("")
  const [timeZone, setTimeZone] = useState<string>("")
  const [timeBlockLengthMin, setTimeBlockLengthMin] = useState<number | null>(0)
  const [timeBlockCostADA, setTimeBlockCostADA] = useState<number | undefined>(
    0
  )
  const [timeZone, setTimeZone] = useState<string | undefined>("")
  const [hourlyRate, setHourlyRate] = useState<HourlyRate>({
    ada: 0,
    gimbals: 0,
  })
  const [profession, setProfession] = useState<string | undefined>("")
  const [jobTitle, setJobTitle] = useState<string | undefined>("")
  const [description, setDescription] = useState<string | undefined>("")
  const [skills, setSkills] = useState<string | undefined>("")
  const [hasSyncedWallet, setHasSyncedWallet] = useState<boolean>(false)
  //@TODO change this...
  const [walletBalance, setWalletBalance] = useState<number>(50)
  const [profileType, setProfileType] = useState<"" | "attendee" | "organizer">(
    ""
  )

  const userProfile = {
    username,
    name,
    id,
    publicKey,
    walletBalance,
    bio,
    imageBase64,
    hasSyncedWallet,
    hourlyRate,
    timeZone,
    timeBlockLengthMin,
    timeBlockCostADA,
    timeZone,
    profession,
    jobTitle,
    description,
    skills,
    profileType,
  }

  const getUserProfile = () => userProfile

  const resetProfileState = () => {
    setUsername("")
    setName("")
    setId("")
    setPublicKey("")
    setBio("")
    setImageBase64("")
    setTimeBlockLengthMin(0)
    setTimeBlockCostADA(0)
    setTimeZone("")
    setHourlyRate({ ada: 0, gimbals: 0 })
    setProfession("")
    setJobTitle("")
    setDescription("")
    setSkills("")
    setTimeZone("")
    setHasSyncedWallet(false)
    setWalletBalance(0)
    setProfileType("")
  }

  return (
    <ProfileContext.Provider
      value={{
        ...userProfile,
        getUserProfile,
        setProfileType,
        setUsername,
        setName,
        setId,
        setPublicKey,
        setBio,
        setImageBase64,
        setWalletBalance,
        setHasSyncedWallet,
        setTimeBlockLengthMin,
        setTimeBlockCostADA,
        setTimeZone,
        setHourlyRate,
        setProfession,
        setJobTitle,
        setDescription,
        setSkills,
        setTimeZone,
        resetProfileState,
      }}>
      {children}
    </ProfileContext.Provider>
  )
}
