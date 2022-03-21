import * as React from "react";
import Svg, { SvgProps, Path } from "react-native-svg";

export const AdaIcon = (props: SvgProps) => {
  return (
    <Svg width={21} height={20} viewBox="0 0 21 20" fill="none" {...props}>
      <Path
        d="M7.095 15.009l3.031-10.122 3.48 10.122M6.753 8.852h6.855m-6.961 2.505h6.96M19.222 10a8.92 8.92 0 01-.692 3.444 8.995 8.995 0 01-1.972 2.92 9.102 9.102 0 01-2.95 1.95 9.179 9.179 0 01-6.961 0 9.102 9.102 0 01-2.95-1.95 8.995 8.995 0 01-1.972-2.92A8.919 8.919 0 011.03 10c0-2.387.958-4.676 2.664-6.364A9.144 9.144 0 0110.126 1c2.412 0 4.726.948 6.431 2.636A8.952 8.952 0 0119.221 10z"
        stroke={props.stroke}
        strokeWidth={props.strokeWidth ? props.strokeWidth : 2}
        strokeLinecap="round"
        strokeLinejoin="round"
      />
    </Svg>
  );
};
