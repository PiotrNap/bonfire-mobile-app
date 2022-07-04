export interface EventBookingDto {
  eventId: string
  bookedDate: Date
  bookedDuration: number
  durationCost: number
  createGoogleCalEvent: boolean
  txHash?: string
}
