declare module "*.svg" {
  import { SvgProps } from "react-native-svg";
  const content: React.StatelessComponent<SvgProps>;
  export default content;
}

// declare namespace JSX {
//   interface IntrinsicAttributes {
//     name?: string;
//     year?: number;
//   }
// }
