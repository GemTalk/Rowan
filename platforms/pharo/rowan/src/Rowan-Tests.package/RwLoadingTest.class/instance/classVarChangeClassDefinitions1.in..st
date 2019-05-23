support - classVarChanges definitions
classVarChangeClassDefinitions1: packageName in: classDefinitionDictionary
	"Class Var Changes: "

	^ classDefinitionDictionary
		at: 'TestClassVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassVarClass'
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