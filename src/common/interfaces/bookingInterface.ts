export interface InitialState {
  pickedDate: Date | number
  duration: number
  durationCost: number
  eventTitle: string
  organizerRate: OrganizerRate | null
  previewingOrganizer: any
  previewingEvent: any
  eventCardInfo: EventCardInfo | null
  maxTimeSlotDuration: number
  minTimeSlotDuration: number
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
  label: string
}

export interface EventLine {
  content: string
  icon?: React.ReactNode
}

export interface EventCardInfo {
  title: string
  image: string
  titleColor: string
  backgroundColor: string
}
