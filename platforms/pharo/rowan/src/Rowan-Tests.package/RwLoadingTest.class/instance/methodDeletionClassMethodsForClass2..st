support - methodDeletion definitions
methodDeletionClassMethodsForClass2: className
  ^ self
    methodDefsFromSpec:
      {{#'testClassMethodSurvivor'.
      'class method to be kept'.
      'testClassMethodSurvivor ^ true'}}