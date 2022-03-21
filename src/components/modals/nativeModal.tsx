import * as React from "react";
import { Text, Pressable } from "react-native";

import ModalSelector from "react-native-modal-selector";
import { Buttons, Sizing, Typography, Outlines } from "styles/index";

export interface NativeModalProps {
  cameraAccessCb: () => any;
  mediaLibraryCb: () => any;
  child: React.ReactNode;
}

export const NativeModal = React.memo(
  ({ cameraAccessCb, mediaLibraryCb, child }: NativeModalProps) => {
    const [selector, setSelector] = React.useState<ModalSelector | null>(null);

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
            onPress={cameraAccessCb}>
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
            onPress={mediaLibraryCb}>
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
    ];

    return (
      <>
        {/* @ts-ignore */}
        <ModalSelector
          closeOnChange={false}
          cancelText="Cancel"
          cancelContainerStyle={{
            paddingVertical: Sizing.x5,
          }}
          ref={(_selector) => setSelector(_selector)}
          data={modalData}>
          {child}
        </ModalSelector>
      </>
    );
  }
);
