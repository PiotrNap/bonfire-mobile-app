/**
 * @description This file defines the set of colors used across the application
 *              as well as function shadeColor for adding 'shade effect' (?)
 */

type Neutral =
  | "white"
  | "s100"
  | "s150"
  | "s200"
  | "s250"
  | "s300"
  | "s400"
  | "s550"
  | "s500"
  | "s600"
  | "s700"
  | "s800"
  | "s900"
  | "black";
export const neutral: Record<Neutral, string> = {
  white: "#ffffff",
  s100: "#efeff6",
  s150: "#dfdfe6",
  s200: "#c7c7ce",
  s250: "#bbbbc2",
  s300: "#9f9ea4",
  s400: "#7c7c82",
  s500: "#6b7280",
  s550: "#575B63",
  s600: "#38383a",
  s700: "#2d2c2e",
  s800: "#212123",
  s900: "#161617",
  black: "#000000",
};

type Primary =
  | "neutral"
  | "s180"
  | "s200"
  | "s300"
  | "s350"
  | "s400"
  | "brand"
  | "s600"
  | "s800";
export const primary: Record<Primary, string> = {
  neutral: "#f5f3ff",
  s180: "#ede9fe",
  s200: "#ddd6fe",
  s300: "#c4b5fd",
  s350: "#a78bfa",
  s400: "#eeddee",
  brand: "#8b5cf6",
  s600: "#6d28d9",
  s800: "#4c1d95",
};

type CalendarCard = "blue" | "yellow";
export const calendarCard: Record<CalendarCard, string> = {
  blue: "#DBEAFE",
  yellow: "#FEF3C7",
};

export const booked = "#FECACA";
export const available = "#DBEAFE";

type Secondary = "brand" | "s200" | "s600";
export const secondary: Record<Secondary, string> = {
  s200: "#b968e8",
  brand: "#591282",
  s600: "#3f0d5c",
};

type Success = "s400";
export const success: Record<Success, string> = {
  s400: "#008a09",
};

type Danger = "s300" | "s400";
export const danger: Record<Danger, string> = {
  s300: "#dd3535",
  s400: "#cf1717",
};

type Warning = "s400";
export const warning: Record<Warning, string> = {
  s400: "#cf9700",
};

export const applyOpacity = (
  hexColor: string,
  opacity: number = 0.8
): string => {
  const red: number = parseInt(hexColor.slice(1, 3), 16),
    green: number = parseInt(hexColor.slice(3, 5), 16),
    blue: number = parseInt(hexColor.slice(5, 7), 16);
  return `rgba(${red},${green},${blue}, ${opacity})`;
};

type Transparent = "clear" | "lightGrey" | "darkGrey";
export const transparent: Record<Transparent, string> = {
  clear: applyOpacity(neutral.black, 0),
  lightGrey: applyOpacity(neutral.s300, 0.4),
  darkGrey: applyOpacity(neutral.s800, 0.8),
};

export const shadeColor = (hexColor: string, percent: number): string => {
  const redGamut: number = parseInt(hexColor.slice(1, 3), 16),
    greenGamut: number = parseInt(hexColor.slice(3, 5), 16),
    blueGamut: number = parseInt(hexColor.slice(5, 7), 16);

  const rgb: number[] = [redGamut, greenGamut, blueGamut];

  const toShadedGamut = (gamut: number): number => {
    return Math.floor(Math.min(gamut * (1 + percent / 100), 255));
  };

  const toHex = (gamut: number): string => {
    const hex = gamut.toString(16);
    return hex.length === 1 ? `0${hex}` : hex;
  };

  const shadedRGB: number[] = rgb.map(toShadedGamut);
  const shadedHex: string[] = shadedRGB.map(toHex);

  const hexString = shadedHex.join("");

  return `#${hexString}`;
};
