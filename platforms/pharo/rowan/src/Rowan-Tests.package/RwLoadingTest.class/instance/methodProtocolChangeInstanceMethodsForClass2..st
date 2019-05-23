support - methodProtocolChange definitions
methodProtocolChangeInstanceMethodsForClass2: className
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethod1'.
      'protocol A'.
      'testInstanceMethod1 ^ true'}.
      {#'testInstanceMethod2'.
      'protocol B'.
      'testInstanceMethod2 ^ true'}}