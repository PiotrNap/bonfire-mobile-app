export const PAGINATED_RESULTS_COUNT = 20

export interface User {
  user_id?: string
}

export interface PaginationRequestDto {
  limit: number
  page: number
}

export interface PaginationResponseDto {
  count: number
  result: any[]
  limit: number
  page: number
}
