support - methodDeletion definitions
methodDeletionInstanceMethodsForClass1: className
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethod'.
      'instance method to be deleted'.
      'testInstanceMethod ^ true'}.
      {#'testInstanceMethodSurvivor'.
      'instance method to be kept'.
      'testInstanceMethodSurvivor ^ true'}}