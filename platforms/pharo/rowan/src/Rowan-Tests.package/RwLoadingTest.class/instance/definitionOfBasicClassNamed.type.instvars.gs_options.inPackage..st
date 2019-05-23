support - basic definitions
definitionOfBasicClassNamed: className type: classType instvars: instvars gs_options: gs_options inPackage: packageName
  ^ self
    definitionOfBasicClassNamed: className
    superclass: 'Object'
    type: classType
    instvars: instvars
    gs_options: gs_options
    inPackage: packageName