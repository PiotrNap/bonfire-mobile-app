/**
 * @name dataContext
 * @desc Context container for the profile data
 */
import React, { useState, createContext } from "react"
import { ProfileState } from "interfaces/profileInterface"

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
  timeBlockLengthMin: 0,
  timeBlockCostADA: 0,
  walletBalance: 0,
  profileType: "",
  hourlyRate: 0,
  getUserProfile: () => {},
  setName: () => {},
  setUsername: () => {},
  setProfileType: () => {},
  setId: () => {},
  setPublicKey: () => {},
  setBio: () => {},
  setImageBase64: () => {},
  setTimeBlockLengthMin: () => {},
  setTimeBlockCostADA: () => {},
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
  const [timeBlockLengthMin, setTimeBlockLengthMin] = useState<number | null>(0)
  const [timeBlockCostADA, setTimeBlockCostADA] = useState<number | undefined>(
    0
  )
  const [hourlyRate, setHourlyRate] = useState<number>(0)
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
    timeBlockLengthMin,
    timeBlockCostADA,
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
    setHourlyRate(0)
    setProfession("")
    setJobTitle("")
    setDescription("")
    setSkills("")
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
        setHourlyRate,
        setProfession,
        setJobTitle,
        setDescription,
        setSkills,
        resetProfileState,
      }}>
      {children}
    </ProfileContext.Provider>
  )
}
