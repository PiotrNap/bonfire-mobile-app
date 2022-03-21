export interface BioState {
  username: string;
  name: string;
  publicKey: string;
  id: string;
  walletBalance: number;
  bio: string;
  imageURL?: string | null;
  hasSyncedWallet: boolean;
  timeBlockLengthMin: number | null;
  timeBlockCostADA: number | undefined;
  profession: string | undefined;
  jobTitle: string | undefined;
  description: string | undefined;
  skills: string | undefined;
  profileType: "attendee" | "organizer" | "";
  setName: (input: string) => void;
  setUsername: (input: string) => void;
  setPublicKey: (input: string) => void;
  setId: (input: string) => void;
  setBio: (input: string) => void;
  setImageURL: (input: string) => void;
  setTimeBlockLengthMin: (input: number) => void;
  setTimeBlockCostADA: (input: number) => void;
  setHasSyncedWallet: (arg: boolean) => void;
  setWalletBalance: (input: number) => void;
  setProfession: (input: string) => void;
  setJobTitle: (input: string) => void;
  setDescription: (input: string) => void;
  setSkills: (input: string) => void;
  setProfileType: (input: "organizer" | "attendee" | "") => void;
}

export interface UserDTO {
  username: string;
  name: string;
  id: string;
  publicKey: string;
}
