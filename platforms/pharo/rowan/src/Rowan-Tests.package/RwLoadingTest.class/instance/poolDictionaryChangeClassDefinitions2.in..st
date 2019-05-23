support - poolDictionaryChanges definitions
poolDictionaryChangeClassDefinitions2: packageName in: classDefinitionDictionary
	"Pool Dictionary Changes: add PD1"

	^ classDefinitionDictionary
		at: 'TestPoolDictionaryClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestPoolDictionaryClass'
								super: 'Object'
								instvars: {}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {(PoolDictionaryDictionary at: #'PoolDictionary1')}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself