export interface PaginationRequestDto {
  limit: number
  page: number
  organizer_id: string
}

export interface PaginationResponseDto {
  count: number
  result: any[]
  limit: number
  page: number
}
