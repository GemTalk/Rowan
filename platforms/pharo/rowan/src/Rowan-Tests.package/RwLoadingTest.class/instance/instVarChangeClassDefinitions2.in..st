support - instVarChanges definitions
instVarChangeClassDefinitions2: packageName in: classDefinitionDictionary
	"Inst Var Changes: add instVar1"

	^ classDefinitionDictionary
		at: 'TestInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestInstVarClass'
								super: 'Object'
								instvars: {'instVar1'}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself