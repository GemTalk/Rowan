support - classInitialization definitions
definitionOfClassInitializationClassNamed: className inPackage: packageName classMethodDefinitions: classMethodDefinitions
  ^ RwClassDefinition
    withProperties:
      (self
        propertiesOfClassInitializationClassNamed: className
        inPackage: packageName)
    classMethodDefinitions: classMethodDefinitions
    instanceMethodDefinitions: Dictionary new