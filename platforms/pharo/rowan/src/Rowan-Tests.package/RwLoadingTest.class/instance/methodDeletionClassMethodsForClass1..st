support - methodDeletion definitions
methodDeletionClassMethodsForClass1: className
  ^ self
    methodDefsFromSpec:
      {{#'testClassMethod'.
      'class method to be deleted'.
      'testClassMethod ^ true'}.
      {#'testClassMethodSurvivor'.
      'class method to be kept'.
      'testClassMethodSurvivor ^ true'}}