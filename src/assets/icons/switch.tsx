import * as React from "react"
import Svg, { SvgProps, Path } from "react-native-svg"

export const SwitchIcon = (props: SvgProps) => (
  <Svg
    fill="none"
    stroke="currentColor"
    strokeWidth={props.strokeWidth || 1.5}
    viewBox="0 0 24 24"
    {...props}>
    <Path
      strokeLinecap="round"
      strokeLinejoin="round"
      d="M7.5 21 3 16.5m0 0L7.5 12M3 16.5h13.5m0-13.5L21 7.5m0 0L16.5 12M21 7.5H7.5"
    />
  </Svg>
)
