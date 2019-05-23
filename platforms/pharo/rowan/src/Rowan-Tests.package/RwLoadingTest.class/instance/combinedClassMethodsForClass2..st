support - combined definitions
combinedClassMethodsForClass2: className
  ^ self
    methodDefsFromSpec:
      {{#'initialize'.
      'initialization'.
      'initialize ClassVar1 := classInstVar1 := true'}.
      {#'testClassMethod1'.
      'xxx'.
      'testClassMethod1 ^ ClassVar1'}.
      {#'testClassMethod3'.
      'yyy'.
      'testClassMethod3 ^ 2'}}