import * as React from "react";
import Svg, { SvgProps, Path } from "react-native-svg";

export const CheckIcon = React.forwardRef((props: SvgProps, ref) => {
  const { strokeWidth, opacity } = props;

  return (
    <Svg
      fill="none"
      opacity={opacity}
      //@ts-ignore
      ref={ref}
      viewBox="0 0 24 24"
      stroke="currentColor"
      {...props}>
      <Path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={strokeWidth ? strokeWidth : 2.4}
        d="M5 13l4 4L19 7"
      />
    </Svg>
  );
});
