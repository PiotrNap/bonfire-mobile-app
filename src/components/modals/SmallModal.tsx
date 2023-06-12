import * as React from "react"
import { Modal } from "react-native"

export interface Props {
  onCloseModal: () => void
  onRequestClose: () => void
  modalVisible: boolean
  children: React.ReactNode
  transparent: boolean
}

export const SmallModal = ({
  modalVisible,
  onCloseModal,
  children,
  transparent,
}: Props) => {
  return (
    <Modal
      animationType="fade"
      visible={modalVisible}
      onDismiss={onCloseModal}
      onRequestClose={onCloseModal}
      transparent={transparent}>
      {children}
    </Modal>
  )
}
