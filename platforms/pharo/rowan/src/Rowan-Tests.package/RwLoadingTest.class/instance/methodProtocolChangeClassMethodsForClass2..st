support - methodProtocolChange definitions
methodProtocolChangeClassMethodsForClass2: className
  ^ self
    methodDefsFromSpec:
      {{#'testClassMethod1'.
      'protocol A'.
      'testClassMethod1 ^ true'}.
      {#'testClassMethod2'.
      'protocol B'.
      'testClassMethod2 ^ true'}}