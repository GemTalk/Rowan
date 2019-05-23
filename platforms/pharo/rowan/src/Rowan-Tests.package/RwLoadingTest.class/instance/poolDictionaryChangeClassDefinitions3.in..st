support - poolDictionaryChanges definitions
poolDictionaryChangeClassDefinitions3: packageName in: classDefinitionDictionary
	"Pool Dictionary Changes: add PD2"

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
								pools:
									{(PoolDictionaryDictionary at: #'PoolDictionary1').
									(PoolDictionaryDictionary at: #'PoolDictionary2')}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself