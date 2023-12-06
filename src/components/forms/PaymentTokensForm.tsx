import React, { useEffect } from "react"
import { useForm, useFieldArray, Controller } from "react-hook-form"
import { ProfileContext } from "contexts/profileContext"
import { Pressable, Button, StyleSheet, View } from "react-native"
import { PlusIcon, RemoveIcon } from "assets/icons"
import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { TokenInput } from "./TokenInput"
import { appContext } from "contexts/contextApi"
import { formStyleDark, formStyleLight, inputStyles } from "../../styles/forms"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { utf8ToHex } from "lib/wallet/utils"

export const PaymentTokensForm = React.forwardRef((props, ref) => {
  const { errorStatus, changeErrorStatus, defaultValues } = props
  const { colorScheme } = appContext()
  const { hourlyRateAda } = React.useContext(ProfileContext)
  const {
    control,
    watch,
    formState: { errors },
  } = useForm({
    defaultValues: {
      paymentTokens: defaultValues.length // defaultValues come from eventCreationContext
        ? defaultValues
        : [
            // 5 ADA is a minimum to cover all possible minUtxos
            {
              policyId: "",
              label: "",
              count: !!hourlyRateAda ? hourlyRateAda : 5,
              name: utf8ToHex("ada"),
              displayName: "ada",
            },
            //@TODO make gimbals here as a default
            // {
            //   policyId: "2b0a04a7b60132b1805b296c7fcb3b217ff14413991bf76f72663c30",
            //   label: "",
            //   count: 100,
            //   name: "gimbal",
            // },
            {
              policyId: "2542e94ef77993cba4594135f2874c8fe19c63fe22760a079552658b",
              label: "",
              count: 100,
              name: utf8ToHex("TestCommunityCoin"),
              displayName: "TestCommunityCoin",
            },
          ],
    },
    shouldUnregister: true,
    mode: "onBlur",
  })
  const isLightMode = colorScheme === "light"
  const formStyles = Object.assign(
    {},
    inputStyles,
    isLightMode ? formStyleLight : formStyleDark
  )
  const fieldValues = watch()

  const { fields, append, remove } = useFieldArray({
    control,
    name: "paymentTokens",
  })

  React.useImperativeHandle(
    ref,
    () => ({
      values: fieldValues.paymentTokens,
      errors,
    }),
    [fieldValues]
  )

  React.useEffect(() => {
    const hasError = !!errors.paymentTokens?.filter?.((er) => er).length
    if (hasError === errorStatus) return
    changeErrorStatus(hasError)
  }, [errors?.paymentTokens?.length])

  return fields.map((field, index) => (
    <View key={field.id}>
      {field.name === utf8ToHex("ada") && (
        <View style={styles.paymentTokensDescriptionWrapper}>
          <SubHeaderText
            colors={[Colors.primary.s800, Colors.primary.neutral]}
            customStyle={styles.paymentTokenDescription}>
            Name
          </SubHeaderText>
          <SubHeaderText
            colors={[Colors.primary.s800, Colors.primary.neutral]}
            customStyle={styles.paymentTokenDescription}>
            PolicyID
          </SubHeaderText>
          <SubHeaderText
            colors={[Colors.primary.s800, Colors.primary.neutral]}
            customStyle={styles.paymentTokenDescription}>
            Quantity
          </SubHeaderText>
        </View>
      )}
      <View style={styles.paymentTokenInputsWrapper}>
        <Controller
          rules={{ required: !!index }}
          control={control}
          name={`paymentTokens[${index}].displayName`}
          render={({ field }) => (
            <TokenInput
              styles={formStyles}
              index={index}
              isDisabled={index === 0}
              errors={errors}
              {...field}
            />
          )}
        />
        <Controller
          rules={{ required: !!index }}
          control={control}
          name={`paymentTokens[${index}].policyId`}
          render={({ field }) => (
            <TokenInput
              styles={formStyles}
              index={index}
              isDisabled={index === 0}
              errors={errors}
              {...field}
            />
          )}
        />
        <Controller
          rules={{
            required: true,
            min: 2,
          }}
          control={control}
          name={`paymentTokens[${index}].count`}
          render={({ field }) => (
            <TokenInput
              styles={formStyles}
              index={index}
              keyboardType={"numeric"}
              errors={errors}
              {...field}
            />
          )}
        />
        <View style={styles.paymentTokenBtnWrapper}>
          {index !== 0 && (
            <Pressable
              hitSlop={Sizing.x10}
              style={Buttons.applyOpacity(styles.removePaymentTokenBtn)}
              onPress={() => remove(index)}>
              <RemoveIcon style={styles.icon} stroke={Colors.neutral.s150} />
            </Pressable>
          )}
        </View>
      </View>
      {index === fields.length - 1 && (
        <View style={styles.addPaymentTokenBtnWrapper}>
          <Pressable
            hitSlop={Sizing.x10}
            style={Buttons.applyOpacity(styles.addPaymentTokenBtn)}
            onPress={() => append({ policyId: "", name: "", count: 0, label: "" })}>
            <PlusIcon style={styles.icon} stroke={Colors.neutral.s150} />
          </Pressable>
        </View>
      )}
    </View>
  ))
})

const styles = StyleSheet.create({
  paymentTokensDescriptionWrapper: {
    flexDirection: "row",
    justifyContent: "space-between",
    marginRight: Sizing.x40,
  },
  paymentTokenDescription: {
    flex: 1,
    ...Typography.subHeader.x10,
    paddingLeft: Sizing.x5,
  },
  paymentTokenInputsWrapper: {
    flex: 1,
    flexDirection: "row",
    marginVertical: Sizing.x5,
  },
  paymentTokenBtnWrapper: {
    width: Sizing.x40,
    alignItems: "center",
    justifyContent: "center",
  },
  removePaymentTokenBtn: {
    width: Sizing.x30,
    height: Sizing.x30,
    backgroundColor: Colors.danger.s400,
    alignItems: "center",
    justifyContent: "center",
    borderRadius: Outlines.borderRadius.max,
  },
  addPaymentTokenBtnWrapper: {
    width: "100%",
    alignItems: "center",
    justifyContent: "center",
    marginTop: Sizing.x5,
    marginBottom: Sizing.x15,
  },
  addPaymentTokenBtn: {
    width: Sizing.x30,
    height: Sizing.x30,
    backgroundColor: "green",
    alignItems: "center",
    justifyContent: "center",
    borderRadius: Outlines.borderRadius.max,
  },
  icon: {
    width: Sizing.x25,
    height: Sizing.x25,
  },
})
