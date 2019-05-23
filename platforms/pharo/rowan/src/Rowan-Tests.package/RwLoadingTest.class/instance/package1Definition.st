support - simple definitions
package1Definition

	^RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> 'TestPackage1'  with: 'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions: (Dictionary
				with: 'TestClass1' -> (self definitionOfSimpleClassNamed: 'TestClass1' inPackage: 'TestPackage1'))
		classExtensions: Dictionary new
