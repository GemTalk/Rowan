support - simple definitions
package2Definition

	^RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> 'TestPackage2'  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions: (Dictionary
				with: 'TestClass2' -> (self definitionOfSimpleClassNamed: 'TestClass2' inPackage: 'TestPackage2'))
		classExtensions: Dictionary new
