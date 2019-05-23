support - basic definitions
basicPackageDefinition: packageName
  ^ RwPackageDefinition
    withProperties: (Dictionary with: 'name' -> packageName with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
    classDefinitions: (self basicClassDefinitions: packageName)
    classExtensions: Dictionary new
