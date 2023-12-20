import React, { createContext, useState } from "react"

type InitialState = {
  addMnemonicInput: (idx: any, el: any) => void
  focusNextField: (el: any) => void
}
const initialState: InitialState = {
  addMnemonicInput: (idx: any, el: any) => {},
  focusNextField: (el: any) => {},
}

export const MnemonicInputsContext = createContext(initialState)

export const MnemonicInputsProvider = ({ children }) => {
  const [inputs, setInputs] = useState([])

  const addMnemonicInput = (idx, inputRef) => {
    setInputs((prevInputs) => {
      const newInputs = [...prevInputs]
      newInputs[idx] = inputRef
      return newInputs
    })
  }
  const focusNextField = (currentIdx) => {
    const nextInput = inputs[currentIdx + 1]
    nextInput?.current?.focus()
  }

  return (
    <MnemonicInputsContext.Provider value={{ addMnemonicInput, focusNextField }}>
      {children}
    </MnemonicInputsContext.Provider>
  )
}
