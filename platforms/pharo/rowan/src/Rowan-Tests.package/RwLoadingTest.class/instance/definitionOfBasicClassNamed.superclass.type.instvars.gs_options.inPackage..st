support - basic definitions
definitionOfBasicClassNamed: className superclass: superclassName type: classType instvars: instvars gs_options: gs_options inPackage: packageName
  ^ RwClassDefinition
    withProperties:
      (self
        propertiesForClassNamed: className
        super: superclassName
        instvars: instvars
        classinstvars: {}
        classvars: {}
        type: classType
        gs_options: gs_options
        inPackage: packageName)
    classMethodDefinitions: (self basicClassMethodsForClass: className)
    instanceMethodDefinitions: (self basicInstanceMethodsForClass: className)