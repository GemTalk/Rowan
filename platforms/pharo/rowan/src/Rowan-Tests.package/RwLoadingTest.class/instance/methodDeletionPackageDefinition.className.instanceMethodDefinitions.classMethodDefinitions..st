support - methodDeletion definitions
methodDeletionPackageDefinition: packageName className: className instanceMethodDefinitions: instanceMethodDefinitions classMethodDefinitions: classMethodDefinitions
  ^ RwPackageDefinition
    withProperties: (Dictionary with: 'name' -> packageName with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
    classDefinitions:
      (Dictionary new
        at: className
          put:
            (self
                definitionOfMethodDeletionClassNamed: className
                inPackage: packageName
                instanceMethodDefinitions: instanceMethodDefinitions
                classMethodDefinitions: classMethodDefinitions);
        yourself)
    classExtensions: Dictionary new
