import { NetworkId } from "lib/wallet/types"

export const PAGINATED_RESULTS_COUNT = 20

export interface User {
  user_id?: string
}

export interface PaginationRequestDto {
  limit: number
  page: number
  network_id: NetworkId
}

export interface PaginationResponseDto {
  count: number
  result: any[]
  limit: number
  page: number
}
