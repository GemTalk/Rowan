support - basic definitions
basicInstanceMethodsForClass: className
  ^ self
    methodDefsFromSpec:
      {{#'instanceMeth'.
      'accessing'.
      ('instanceMeth ^''instanceMeth for class ''' , className printString)}}