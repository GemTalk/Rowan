support - combined definitions
combinedClassMethodsForClass1: className
  ^ self
    methodDefsFromSpec:
      {{#'initialize'.
      'initialization'.
      'initialize ClassVar1 := classInstVar1 := true'}.
      {#'testClassMethod1'.
      'xxx'.
      'testClassMethod1 ^ ClassVar1'}.
      {#'testClassMethod2'.
      'xxx'.
      'testClassMethod2 ^ classInstVar1'}.
      {#'testClassMethod3'.
      'xxx'.
      'testClassMethod3 ^ 1'}}