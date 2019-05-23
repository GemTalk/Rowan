support - combined definitions
combinedInstanceMethodsForClass2: className
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethod1'.
      'xxx'.
      'testInstanceMethod1 ^ true'}.
      {#'testInstanceMethod3'.
      'yyy'.
      'testInstanceMethod3 ^ 2'}}