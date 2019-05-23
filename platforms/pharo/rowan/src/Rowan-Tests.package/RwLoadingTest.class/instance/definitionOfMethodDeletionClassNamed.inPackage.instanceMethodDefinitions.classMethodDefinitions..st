support - methodDeletion definitions
definitionOfMethodDeletionClassNamed: className inPackage: packageName instanceMethodDefinitions: instanceMethodDefinitions classMethodDefinitions: classMethodDefinitions
  ^ RwClassDefinition
    withProperties:
      (self
        propertiesForClassNamed: className
        super: 'Object'
        instvars: {}
        classinstvars: {}
        classvars: {}
        type: 'normal'
        gs_options: {}
        inPackage: packageName)
    classMethodDefinitions: classMethodDefinitions
    instanceMethodDefinitions: instanceMethodDefinitions