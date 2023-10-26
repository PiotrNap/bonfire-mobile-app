import * as React from "react"
import Svg, { SvgProps, Path } from "react-native-svg"

export const DownArrow = (props: SvgProps) => (
  <Svg
    fill="none"
    stroke="currentColor"
    strokeWidth={props.strokeWidth ?? 1.5}
    viewBox="0 0 24 24"
    {...props}>
    <Path
      strokeLinecap="round"
      strokeLinejoin="round"
      d="M19.5 13.5 12 21m0 0-7.5-7.5M12 21V3"
    />
  </Svg>
)
