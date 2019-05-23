support - basic definitions
basicClassMethodsForClass: className
  ^ self
    methodDefsFromSpec:
      {{#'classMeth'.
      'accessing'.
      ('classMeth ^''classMeth for class ''' , className printString)}}