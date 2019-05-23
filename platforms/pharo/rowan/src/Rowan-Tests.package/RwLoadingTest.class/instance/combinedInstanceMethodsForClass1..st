support - combined definitions
combinedInstanceMethodsForClass1: className
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethod1'.
      'xxx'.
      'testInstanceMethod1 ^ true'}.
      {#'testInstanceMethod2'.
      'xxx'.
      'testInstanceMethod2 ^ true'}.
      {#'testInstanceMethod3'.
      'xxx'.
      'testInstanceMethod3 ^ 1'}}