import * as SC from "expo-secure-store";

// Just to minimize chance of collision with other apps when
// encryptes storage is shared among other apps.
const generateKey = (val: any) => `${val}_onetoone`;

export const setToEncryptedStorage = async (
  key: string,
  value: any
): Promise<void> => {
  try {
    value = JSON.stringify(value);

    await SC.setItemAsync(generateKey(key), value);
  } catch (e) {
    console.error(e);
    throw new Error(e);
  }
};

export const removeFromEncryptedStorage = async (key: string) => {
  try {
    await SC.deleteItemAsync(generateKey(key));
  } catch (e) {
    throw new Error(e);
  }
};

export const isAvailableEncryptedStorage = async (): Promise<boolean> => {
  try {
    return await SC.isAvailableAsync();
  } catch {
    return false;
  }
};

export const getFromEncryptedStorage = async (key: string): Promise<any> => {
  try {
    const val = await SC.getItemAsync(generateKey(key));

    if (val != null) {
      return JSON.parse(val);
    } else {
      return null;
    }
  } catch (e) {
    throw new Error(e);
  }
};
