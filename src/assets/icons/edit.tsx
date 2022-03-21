import * as React from "react";
import Svg, { SvgProps, Path } from "react-native-svg";

export const EditIcon = (props: SvgProps) => {
  return (
    <Svg
      viewBox="0 0 24 24"
      stroke="currentColor"
      strokeWidth={2}
      strokeLinecap="round"
      strokeLinejoin="round"
      {...props}>
      <Path d="M12 20h9M16.5 3.5a2.121 2.121 0 013 3L7 19l-4 1 1-4L16.5 3.5z" />
    </Svg>
  );
};
