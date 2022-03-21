import * as React from "react";
import Svg, { SvgProps, Circle } from "react-native-svg";

export const DotIcon = (props: SvgProps) => {
  return (
    <Svg
      viewBox="0 0 24 24"
      fill="currentColor"
      stroke="currentColor"
      strokeWidth={2}
      strokeLinecap="round"
      strokeLinejoin="round"
      {...props}>
      <Circle cx={12} cy={12} r={10} />
    </Svg>
  );
};
