-- A specification of a menu command, which can be used to invoke a particular message
{ keyCmd : Optional ./KeyboardShortcut.typ
, lineCmd : Optional Text
  -- ^ Command line invocation
, menuCmd : Optional Text
  -- ^ Menu command invocation
, withCtrl : Optional Bool
  -- ^ If the command has to be invoked with CTRL pressed; if `Nothing`, it
  -- can be invoked whether CTRl is pressed or not; applies to `keyCmd` and
  -- `menuCmd`
, help : Optional Text
  -- ^ The corresponding help string
}
