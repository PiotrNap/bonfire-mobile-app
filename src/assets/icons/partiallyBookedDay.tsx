import * as React from "react";
import Svg, {
  SvgProps,
  Ellipse,
  Defs,
  LinearGradient,
  Stop,
} from "react-native-svg";

export const PartiallyBookedDay = (props: SvgProps) => {
  return (
    <Svg width={34} height={34} viewBox="0 0 34 34" fill="none" {...props}>
      <Ellipse cx={17.005} cy={17.217} rx={16.271} ry={16.28} fill="#DBEAFE" />
      <Ellipse
        cx={17.005}
        cy={17.217}
        rx={16.271}
        ry={16.28}
        fill="url(#prefix__paint0_linear)"
      />
      <Defs>
        <LinearGradient
          id="prefix__paint0_linear"
          x1={17.005}
          y1={0.937}
          x2={17.005}
          y2={33.497}
          gradientUnits="userSpaceOnUse">
          <Stop offset={0.495} stopColor="#EDE9FE" />
          <Stop offset={0.502} stopColor="#FECACA" />
        </LinearGradient>
      </Defs>
    </Svg>
  );
};
