export interface PaginationRequestDto {
  limit: number;
  page: number;
}

export interface PaginationResponseDto {
  count: number;
  result: any[];
  limit: number;
  page: number;
}
