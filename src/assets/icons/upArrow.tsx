import * as React from "react"
import Svg, { SvgProps, Path } from "react-native-svg"

export const UpArrow = (props: SvgProps) => (
  <Svg
    fill="none"
    stroke="currentColor"
    strokeWidth={props.strokeWidth ?? 1.5}
    viewBox="0 0 24 24"
    {...props}>
    <Path
      strokeLinecap="round"
      strokeLinejoin="round"
      d="M4.5 10.5 12 3m0 0 7.5 7.5M12 3v18"
    />
  </Svg>
)
