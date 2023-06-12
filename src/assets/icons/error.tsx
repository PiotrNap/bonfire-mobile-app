import * as React from "react"
import Svg, { SvgProps, Path } from "react-native-svg"

export const ErrorIcon = (props: SvgProps): JSX.Element => {
  return (
    <Svg fill="none" viewBox="0 0 24 24" stroke="currentColor" {...props}>
      <Path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={props.strokeWidth ?? 2}
        d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
      />
    </Svg>
  )
}
