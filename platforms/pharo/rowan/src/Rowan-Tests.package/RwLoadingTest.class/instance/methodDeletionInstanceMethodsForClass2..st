support - methodDeletion definitions
methodDeletionInstanceMethodsForClass2: className
  ^ self
    methodDefsFromSpec:
      {{#'testInstanceMethodSurvivor'.
      'instance method to be kept'.
      'testInstanceMethodSurvivor ^ true'}}