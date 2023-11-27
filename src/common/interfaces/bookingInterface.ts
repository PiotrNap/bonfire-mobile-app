import { EventSlot } from "./newEventInterface"
import { AssetUnit } from "lib/wallet/types"

export interface InitialState {
  pickedDate: string // eg. '2023-11-29'
  pickedDateSlots: EventSlot[]
  pickedDateSlotsMinDuration: number[]
  pickedStartTime: string
  duration: number
  durationCost: HourlyRate
  eventTitle: string
  organizerRate: OrganizerRate | null
  previewingOrganizer: any
  previewingEvent: any
  eventCardInfo: EventCardInfo | null
  maxTimeSlotDuration: number
  minTimeSlotDuration: number
  createGoogleCalEvent: boolean
}

export interface OrganizerRate {
  timeBlockCost: number
  timeBlockLength: number
}

export interface BookingContextProps {
  state: InitialState
  dispatch: React.Dispatch<any>
}

export interface SectionDetail {
  lineContent: EventLine | EventLine[] | undefined
  label: string
  callbackFn?: EventCallbackFn | undefined
  isLastItem?: boolean
}

export interface EventCallbackFn {
  callbackFnScreen: string
  onPress: () => void
  icon?: React.ReactNode
  label: string
  param?: any
}

export interface EventLine {
  content: string
  icon?: React.ReactNode
  icons?: React.ReactNode[]
}

export interface EventCardInfo {
  title: string
  image: string
  titleColor: string
  backgroundColor: string
}
export type HourlyRate = Map<string, AssetUnit | number>
export type TimeIncrement = {
  hours: string
  millSeconds: number
}
