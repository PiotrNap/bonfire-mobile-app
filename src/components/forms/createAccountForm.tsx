import * as React from "react";
import { Animated, Pressable, StyleSheet, Text, View } from "react-native";

import { Formik, Field } from "formik";
import {
  Buttons,
  Typography,
  Colors,
  Sizing,
  Forms,
  Outlines,
} from "styles/index";
import { createAccountValidationScheme } from "lib/utils";
import { CustomInput } from "../forms/CustomInput";
import { Users } from "Api/Users";
import { setAuthorizationToken } from "Api/base";
import { CheckIcon } from "icons/index";
import { FullWidthButton } from "components/buttons/fullWidthButton";
import { setToEncryptedStorage } from "lib/encryptedStorage";
import { ProfileContext } from "contexts/profileContext";
import { useNavigation } from "@react-navigation/native";
import { appContext } from "contexts/contextApi";
import { startChallengeSequence } from "lib/helpers";
import { generateKeyPair } from "lib/tweetnacl";
import base64 from "base64-js";

export interface CreateAccountFormProps {
  onErrorCallback: (val: string) => void;
  onChangeCallback: () => void;
}

const AnimatedCheckIcon = Animated.createAnimatedComponent(CheckIcon);

export const CreateAccountForm = ({
  onErrorCallback,
  onChangeCallback,
}: CreateAccountFormProps) => {
  const { profileType, setUsername, setId, setName } =
    React.useContext(ProfileContext);
  const { toggleAuth } = appContext();
  const [acceptedCheckbox, setAcceptedChecbox] = React.useState<boolean>(false);
  const [submitted, setSubmitted] = React.useState<boolean>(false);
  const [isLoading, setIsLoading] = React.useState<boolean>(false);
  const animatedOpacity = React.useRef(new Animated.Value(0)).current;
  const navigation = useNavigation();

  const fadeIn = () => {
    Animated.timing(animatedOpacity, {
      toValue: 1,
      duration: 0,
      useNativeDriver: true,
    }).start(({ finished }) => {
      if (finished) setAcceptedChecbox(true);
    });
  };
  const fadeOut = () => {
    Animated.timing(animatedOpacity, {
      toValue: 0,
      duration: 0,
      useNativeDriver: true,
    }).start(({ finished }) => {
      if (finished) setAcceptedChecbox(false);
    });
  };

  React.useEffect(() => {
    setAcceptedChecbox(false);
    fadeOut();
  }, [profileType]);

  const onCheckBoxPress = () => {
    onChangeCallback();
    !acceptedCheckbox ? fadeIn() : fadeOut();
  };

  const onSubmit = async (values: any) => {
    if (!acceptedCheckbox) {
      onErrorCallback("AcceptedTerms");
      return;
    }
    setIsLoading(true);

    // generate key pair
    const keyPair = await generateKeyPair();
    const secretKey = keyPair?.secretKey;
    const publicKey = keyPair?.publicKey;

    if (publicKey && secretKey) {
      // store private key in encrypted storage as base64
      setToEncryptedStorage("secret", base64.fromByteArray(secretKey));
      setToEncryptedStorage("public", base64.fromByteArray(publicKey));

      try {
        values.publicKey = base64.fromByteArray(publicKey);
        values.profileType = profileType;

        // get new user object
        const user = await Users.createAccount(values);

        if (user != null) {
          const { id, name, username } = user;

          setId(id);
          setUsername(username);
          setName(name);

          // start challenge and get JWT
          const loginResponseDTO = await startChallengeSequence(id, true);

          if (loginResponseDTO) {
            const { accessToken } = loginResponseDTO;
            setAuthorizationToken(accessToken);
            setToEncryptedStorage("accessToken", accessToken);
          }

          setSubmitted(true);
          toggleAuth(true, profileType);

          if (profileType === "attendee") {
            navigation.navigate("Navigation Screens");
          } else if (profileType === "organizer") {
            navigation.navigate("User Registration Screens");
          }

          setIsLoading(false);
        }
      } catch (e: any) {
        // show error notification
        if (e.message === "User already exists") {
          onErrorCallback("UserNameTaken");
        } else {
          console.error(e);
          onErrorCallback("Server");
        }

        setIsLoading(false);
      }
    }
  };

  return (
    <Formik
      validationSchema={createAccountValidationScheme()}
      validateOnChange={submitted}
      validateOnBlur={submitted}
      initialValues={{
        name: "",
        username: "",
      }}
      onSubmit={onSubmit}>
      {({ handleSubmit, isValid, validateForm }) => (
        <>
          <Field
            key="name"
            name="name"
            label="Name"
            component={CustomInput}
            onChange={onChangeCallback}
            placeholder="John Doe"
            keyboardType="default"
            textContentType="name"
            autoCompleteType="name"
            validateForm={validateForm}
            submitted={submitted}
            styles={inputStyles}
          />
          <Field
            key="Username"
            name="username"
            label="Username"
            component={CustomInput}
            onChange={onChangeCallback}
            placeholder="@John12"
            keyboardType="default"
            textContentType="username"
            autoCompleteType="username"
            validateForm={validateForm}
            submitted={submitted}
            styles={inputStyles}
          />
          <View style={styles.checkboxWrapper}>
            <Pressable
              onPress={onCheckBoxPress}
              style={styles.pressableCheckbox}
              hitSlop={2}>
              <View style={styles.checkbox}>
                <AnimatedCheckIcon
                  opacity={animatedOpacity}
                  width="15"
                  height="15"
                  strokeWidth="3.5"
                  stroke={Colors.primary.s600}
                />
              </View>
            </Pressable>
            <Text style={styles.checkboxText}>I accept</Text>
            <Text style={styles.checkboxTextLink}>
              Privacy Policy + Terms of Use
            </Text>
          </View>
          <View style={styles.appendix}>
            <Text style={styles.appendixText}>Already have an account?</Text>
            <Text style={styles.appendixTextLink}>Sign in</Text>
          </View>
          <FullWidthButton
            colorScheme={"dark"}
            buttonType="transparent"
            loadingIndicator={isLoading}
            onPressCallback={handleSubmit}
            style={styles.submitButton}
            text={"Create account"}
            textStyle={styles.submitButtonText}
            disabled={!isValid}
          />
        </>
      )}
    </Formik>
  );
};

const styles = StyleSheet.create({
  formInput: {
    ...Forms.input.primary,
  },
  formLabel: {
    ...Forms.inputLabel.primary,
  },
  submitButton: {
    ...Buttons.bar.transparent_dark,
  },
  submitButtonText: {
    ...Buttons.barText.transparent_dark,
  },
  checkboxWrapper: {
    width: "100%",
    flexDirection: "row",
    alignItems: "center",
    marginTop: Sizing.x5,
  },
  pressableCheckbox: {
    width: 32,
    height: 32,
    alignItems: "center",
    justifyContent: "center",
  },
  checkbox: {
    alignItems: "center",
    justifyContent: "center",
    width: 17,
    height: 17,
    padding: Sizing.x1,
    borderRadius: Sizing.x3,
    backgroundColor: Colors.primary.neutral,
  },
  checkboxText: {
    marginLeft: Sizing.x2,
    marginRight: Sizing.x5,
    ...Typography.body.x30,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.neutral,
  },
  checkboxTextLink: {
    ...Typography.body.x30,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.s300,
  },
  appendix: {
    width: "100%",
    flexDirection: "row",
    alignItems: "center",
    marginTop: Sizing.x10,
    paddingBottom: Sizing.x5,
  },
  appendixText: {
    marginLeft: Sizing.x10,
    marginRight: Sizing.x5,
    ...Typography.body.x30,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.neutral,
  },
  appendixTextLink: {
    ...Typography.body.x30,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.s300,
  },
});

/**
 * Styles passed as props to CustomInput
 */
const inputStyles = StyleSheet.create({
  inputContainer: {
    width: "100%",
    alignItems: "center",
  },
  labelContainer: {
    width: "100%",
  },
  label: {
    ...Forms.inputLabel.primary,
  },
  textInputWrapper: {
    width: "100%",
    flexDirection: "row",
    alignItems: "center",
  },
  input: {
    width: "100%",
    ...Forms.input.primary,
  },
  placeholderText: {
    color: Colors.primary.s300,
  },
  errorInput: {
    borderColor: Colors.danger.s300,
  },
  errorWrapper: {
    alignSelf: "center",
    height: 22, // inspect element in expo to see how much pixels it needs
    paddingHorizontal: Sizing.x8,
    marginTop: Sizing.x5,
    justifyContent: "center",
    backgroundColor: Colors.danger.s300,
    borderRadius: Outlines.borderRadius.base,
  },
  error: {
    ...Forms.inputLabel.error,
    color: Colors.primary.neutral,
  },
});
