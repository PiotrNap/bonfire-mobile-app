import * as React from "react"
import Svg, { SvgProps, Path } from "react-native-svg"

export const CalendarIcon = React.forwardRef((props: SvgProps, ref) => {
  return (
    <Svg
      //@ts-ignore
      ref={ref}
      fill="none"
      viewBox="0 0 24 24"
      stroke="currentColor"
      {...props}>
      <Path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={props.strokeWidth ? props.strokeWidth : 1.7}
        d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"
      />
    </Svg>
  )
})
