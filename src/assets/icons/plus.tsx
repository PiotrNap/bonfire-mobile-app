import * as React from "react"
import Svg, { SvgProps, Path } from "react-native-svg"

export const PlusIcon = (props: SvgProps) => {
  return (
    <Svg fill="none" viewBox="0 0 24 24" stroke="currentColor" {...props}>
      <Path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={props.strokeWidth ? props.strokeWidth : 2}
        d="M12 4v16m8-8H4"
      />
    </Svg>
  )
}
