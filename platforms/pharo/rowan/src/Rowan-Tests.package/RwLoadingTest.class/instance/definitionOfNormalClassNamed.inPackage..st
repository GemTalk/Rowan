support - basic definitions
definitionOfNormalClassNamed: className inPackage: packageName
  ^ self
    definitionOfBasicClassNamed: className
    type: 'normal'
    instvars: {}
    gs_options: {}
    inPackage: packageName