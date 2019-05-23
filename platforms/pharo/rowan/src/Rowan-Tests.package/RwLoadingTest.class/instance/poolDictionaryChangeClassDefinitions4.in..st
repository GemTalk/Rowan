support - poolDictionaryChanges definitions
poolDictionaryChangeClassDefinitions4: packageName in: classDefinitionDictionary
	"Pool Dictionary Changes: add PD3 and remove PD1"

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
									{(PoolDictionaryDictionary at: #'PoolDictionary2').
									(PoolDictionaryDictionary at: #'PoolDictionary3')}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself