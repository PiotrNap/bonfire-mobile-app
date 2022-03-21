export interface EventBookingDto {
  eventId: string;
  bookedDate: Date;
  bookedDuration: number;
  txHash?: string;
}
