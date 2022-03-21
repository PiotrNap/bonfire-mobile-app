import * as React from "react";
import Svg, { SvgProps, Path } from "react-native-svg";

export const DownIcon = React.forwardRef((props: SvgProps, ref) => {
  return (
    <Svg
      fill="none"
      viewBox="0 0 24 24"
      stroke="currentColor"
      //@ts-ignore
      ref={ref}
      {...props}>
      <Path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={1.8}
        d="M19 9l-7 7-7-7"
      />
    </Svg>
  );
});
