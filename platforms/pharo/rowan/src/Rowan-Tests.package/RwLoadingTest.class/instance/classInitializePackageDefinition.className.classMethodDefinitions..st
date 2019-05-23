support - classInitialization definitions
classInitializePackageDefinition: packageName className: className classMethodDefinitions: classMethodDefinitions
  ^ RwPackageDefinition
    withProperties: (Dictionary with: 'name' -> packageName with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
    classDefinitions:
      (Dictionary new
        at: className
          put:
            (self
                definitionOfClassInitializationClassNamed: className
                inPackage: packageName
                classMethodDefinitions: classMethodDefinitions);
        yourself)
    classExtensions: Dictionary new
