import * as React from "react"
import Svg, { SvgProps, Path } from "react-native-svg"

export const PencilAltIcon = React.forwardRef((props: SvgProps, ref) => {
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
        strokeWidth={props.strokeWidth ?? 1.7}
        d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z"
      />
    </Svg>
  )
})
