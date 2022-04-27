import { Users } from "Api/Users"
import { ProfileContext } from "contexts/profileContext"
import * as React from "react"
import { Text, Pressable, Alert } from "react-native"

import ModalSelector from "react-native-modal-selector"
import { Buttons, Sizing, Typography } from "styles/index"

export interface NativeModalProps {
  cameraAccessCb: (ref: any) => any
  mediaLibraryCb: (ref: any) => any
  onImageDeleted: () => void
  child: React.ReactNode
}

export const NativeModal = ({
  cameraAccessCb,
  mediaLibraryCb,
  onImageDeleted,
  child,
}: NativeModalProps) => {
  const { id } = React.useContext(ProfileContext)
  const [selector, setSelector] = React.useState<ModalSelector | null>(null)

  const removeUserImage = async () => {
    const res = await Users.deleteUserImage(id)
    if (res?.status !== 200) {
      Alert.alert(
        "Error happened",
        `We've encountered some troubles while trying
                 to delete your image, please try again later.`,
        [
          {
            text: "OK",
            style: "default",
          },
        ]
      )
    } else {
      Alert.alert("Success!", `Your image was updated successfully.`, [
        {
          text: "Close",
          style: "default",
          onPress: onImageDeleted,
        },
      ])
    }
  }

  const modalData = [
    {
      key: 1,
      testID: "1-take-photo",
      label: "Take Photo",
      component: (
        <Pressable
          style={Buttons.applyOpacity({
            alignItems: "center",
            justifyContent: "center",
            height: Sizing.x35,
          })}
          hitSlop={10}
          onPress={() => cameraAccessCb(selector)}>
          <Text
            style={{
              ...Typography.subHeader.x25,
              textAlignVertical: "center",
              includeFontPadding: true,
              textAlign: "justify",
            }}>
            Take photo
          </Text>
        </Pressable>
      ),
    },
    {
      key: 2,
      testID: "2-take-photo",
      label: "Browse...",
      component: (
        <Pressable
          style={Buttons.applyOpacity({
            alignItems: "center",
            justifyContent: "center",
            height: Sizing.x30,
          })}
          hitSlop={10}
          onPress={() => mediaLibraryCb(selector)}>
          <Text
            style={{
              ...Typography.subHeader.x25,
              textAlignVertical: "center",
              includeFontPadding: true,
              textAlign: "justify",
            }}>
            Browse...
          </Text>
        </Pressable>
      ),
    },
    {
      key: 3,
      testID: "3-take-photo",
      label: "Delete",
      component: (
        <Pressable
          style={Buttons.applyOpacity({
            alignItems: "center",
            justifyContent: "center",
            height: Sizing.x30,
          })}
          hitSlop={10}
          onPress={removeUserImage}>
          <Text
            style={{
              ...Typography.subHeader.x25,
              textAlignVertical: "center",
              includeFontPadding: true,
              textAlign: "justify",
            }}>
            Delete
          </Text>
        </Pressable>
      ),
    },
  ]

  return (
    <>
      {/* @ts-ignore */}
      <ModalSelector
        closeOnChange={true}
        cancelText="Cancel"
        cancelContainerStyle={{
          paddingVertical: Sizing.x5,
        }}
        ref={(_selector) => setSelector(_selector)}
        data={modalData}>
        {child}
      </ModalSelector>
    </>
  )
}
