import * as React from "react";
import Svg, { SvgProps, Circle } from "react-native-svg";

type SingleColorDot = SvgProps & { color: string };

export const SingleColorDot = (props: SvgProps) => {
  return (
    <Svg width={15} height={15} viewBox="0 0 15 15" fill="none" {...props}>
      <Circle cx={7.5} cy={7.5} r={7} fill={props.color} stroke="#fff" />
    </Svg>
  );
};
