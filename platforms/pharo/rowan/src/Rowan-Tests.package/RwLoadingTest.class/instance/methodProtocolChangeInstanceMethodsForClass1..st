support - methodProtocolChange definitions
methodProtocolChangeInstanceMethodsForClass1: className
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethod1'.
      'protocol A'.
      'testInstanceMethod1 ^ true'}.
      {#'testInstanceMethod2'.
      'protocol A'.
      'testInstanceMethod2 ^ true'}}