support - instVarChanges definitions
instVarChangeClassDefinitions1: packageName in: classDefinitionDictionary
	"Inst Var Changes: "

	^ classDefinitionDictionary
		at: 'TestInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestInstVarClass'
								super: 'Object'
								instvars: {}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself