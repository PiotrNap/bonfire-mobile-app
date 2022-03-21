import * as React from "react";
import Svg, {
  SvgProps,
  Circle,
  Defs,
  LinearGradient,
  Stop,
} from "react-native-svg";

type DoubleColorDot = SvgProps & {
  firstColor: string;
  secondColor: string;
};

export const DoubleColorDot = (props: DoubleColorDot) => {
  return (
    <Svg width={15} height={15} viewBox="0 0 15 15" fill="none" {...props}>
      <Circle cx={7.5} cy={7.5} r={7} fill={props.firstColor} />
      <Circle cx={7.5} cy={7.5} r={7} fill="url(#prefix__paint0_linear)" />
      <Circle cx={7.5} cy={7.5} r={7} stroke="#fff" />
      <Defs>
        <LinearGradient
          id="prefix__paint0_linear"
          x1={7.5}
          y1={0}
          x2={7.5}
          y2={15}
          gradientUnits="userSpaceOnUse">
          <Stop offset={0.495} stopColor={props.firstColor} />
          <Stop offset={0.502} stopColor={props.secondColor} />
        </LinearGradient>
      </Defs>
    </Svg>
  );
};
