import { useWindowDimensions } from "react-native";

type Screen = "height" | "width";
export const screen = (): Record<Screen, number> => {
  return {
    height: useWindowDimensions().height,
    width: useWindowDimensions().width,
  };
};

type Layout =
  | "x1"
  | "x2"
  | "x3"
  | "x5"
  | "x7"
  | "x8"
  | "x9"
  | "x10"
  | "x12"
  | "x14"
  | "x15"
  | "x20"
  | "x25"
  | "x30"
  | "x35"
  | "x40"
  | "x42"
  | "x45"
  | "x50"
  | "x55"
  | "x60"
  | "x65"
  | "x70"
  | "x80"
  | "x85"
  | "x90"
  | "x100"
  | "x110"
  | "x120"
  | "x130"
  | "x140"
  | "x150";
export const layout: Record<Layout, number> = {
  x1: 1,
  x2: 2,
  x3: 3,
  x5: 5,
  x7: 7,
  x9: 9,
  x8: 8,
  x10: 10,
  x12: 11,
  x14: 13,
  x15: 14,
  x20: 18,
  x25: 22,
  x30: 27,
  x35: 30,
  x40: 34,
  x42: 36,
  x45: 38,
  x50: 42,
  x55: 46,
  x60: 50,
  x65: 60,
  x70: 64,
  x80: 86,
  x85: 108,
  x90: 120,
  x100: 130,
  x110: 150,
  x120: 170,
  x130: 200,
  x140: 230,
  x150: 250,
};

export const x1 = layout.x1;
export const x2 = layout.x2;
export const x3 = layout.x3;
export const x5 = layout.x5;
export const x7 = layout.x7;
export const x8 = layout.x8;
export const x9 = layout.x9;
export const x10 = layout.x10;
export const x12 = layout.x12;
export const x14 = layout.x14;
export const x15 = layout.x15;
export const x20 = layout.x20;
export const x25 = layout.x25;
export const x30 = layout.x30;
export const x35 = layout.x35;
export const x40 = layout.x40;
export const x42 = layout.x42;
export const x45 = layout.x45;
export const x50 = layout.x50;
export const x55 = layout.x55;
export const x60 = layout.x60;
export const x65 = layout.x65;
export const x70 = layout.x70;
export const x80 = layout.x80;
export const x85 = layout.x85;
export const x90 = layout.x90;
export const x100 = layout.x100;
export const x110 = layout.x110;
export const x120 = layout.x120;
export const x130 = layout.x130;
export const x140 = layout.x140;
export const x150 = layout.x150;

type Icons = "x10" | "x15" | "x20" | "x25" | "x30" | "x40";
export const icons: Record<Icons, number> = {
  x10: 14,
  x15: 17,
  x20: 20,
  x25: 25,
  x30: 30,
  x40: 40,
};

type IconStroke = "x1" | "x2";
export const iconStroke: Record<IconStroke, number> = {
  x1: 1,
  x2: 2,
};
