support - instVarChanges definitions
instVarChangeClassDefinitions3: packageName in: classDefinitionDictionary
	"Inst Var Changes: add instVar2"

	^ classDefinitionDictionary
		at: 'TestInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestInstVarClass'
								super: 'Object'
								instvars:
									{'instVar1'.
									'instVar2'}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself