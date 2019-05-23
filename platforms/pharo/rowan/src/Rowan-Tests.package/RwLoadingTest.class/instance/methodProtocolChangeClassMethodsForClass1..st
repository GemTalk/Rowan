support - methodProtocolChange definitions
methodProtocolChangeClassMethodsForClass1: className
  ^ self
    methodDefsFromSpec:
      {{#'testClassMethod1'.
      'protocol A'.
      'testClassMethod1 ^ true'}.
      {#'testClassMethod2'.
      'protocol A'.
      'testClassMethod2 ^ true'}}