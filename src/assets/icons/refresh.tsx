import * as React from "react";
import Svg, { SvgProps, Path } from "react-native-svg";

export const RefreshIcon = (props: SvgProps) => {
  return (
    <Svg width={18} height={19} viewBox="0 0 18 19" {...props}>
      <Path
        d="M1.582 7H1V2l.582 5zm0 0a8.001 8.001 0 013.383-3.91M1.582 7H6M1.582 7a7.974 7.974 0 013.383-3.91m9.704 1.264A8.001 8.001 0 0116.938 9a7.949 7.949 0 00-2.269-4.646zm0 0A8.001 8.001 0 004.965 3.09M16.419 13H17v5l-.581-5zm0 0a8.002 8.002 0 01-3.384 3.907M16.419 13H12m4.419 0a8.053 8.053 0 01-3.384 3.907m0 0a8.002 8.002 0 01-9.702-1.263m0 0a8.003 8.003 0 010 0z"
        strokeWidth={props.strokeWidth}
        strokeLinecap="round"
        strokeLinejoin="round"
      />
    </Svg>
  );
};
