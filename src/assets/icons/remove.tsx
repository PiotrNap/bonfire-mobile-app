import * as React from "react";
import Svg, { Path, SvgProps } from "react-native-svg";

export const RemoveIcon = (props: SvgProps) => {
  return (
    <Svg
      viewBox="0 0 24 24"
      stroke="currentColor"
      strokeWidth={2}
      strokeLinecap="round"
      strokeLinejoin="round"
      {...props}>
      <Path d="M18 6L6 18M6 6l12 12" />
    </Svg>
  );
};
