import AsyncStorage from "@react-native-async-storage/async-storage";

export const getValue = async (name: string): Promise<any> => {
  try {
    const value = await AsyncStorage.getItem(name);
    if (value != null) {
      return JSON.parse(value);
    }
  } catch (err) {
    // do something with the error
  }
};

export const setValue = async (
  name: string,
  value: any
): Promise<boolean | void> => {
  if (typeof value === "object") {
    value = JSON.stringify(value);
  }

  try {
    await AsyncStorage.setItem(name, value);
    return true;
  } catch (err) {
    // do something with the error
  }
};

export const removeValue = async (name: string): Promise<boolean | void> => {
  try {
    await AsyncStorage.removeItem(name);
    return true;
  } catch (err) {
    // do something with the error
  }
};
