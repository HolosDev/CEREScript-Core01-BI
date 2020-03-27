

modifyValue :: CERESOperator -> IM.IntMap Value -> Value -> Value
modifyValue operator localEnv value = newValue
 where
  mOperand = error "[FIXME]<modifyValue> Use second argument"
  newValue = maybe (ErrValue "Null register 0")
                   (operatorSelector operator value)
                   mOperand


modifyValueStack :: [CERESOperator] -> IM.IntMap Value -> Value -> (IM.IntMap Value , Value)
modifyValueStack []                     localEnv value = (localEnv, value)
modifyValueStack (operator : operators) localEnv value = modifyValueStack
  operators
  newLocalEnv
  newValue
 where
  newValue    = error "[FIXME]<modifyValueStack> Not yet implemented"
  newLocalEnv = error "[FIXME]<modifyValueStack> Not yet implemented"

bitmaskWithRejection64
