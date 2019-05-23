support - classInstVarChanges definitions
classInstVarChangeClassDefinitions1: packageName in: classDefinitionDictionary
	"Class Inst Var Changes: "

	^ classDefinitionDictionary
		at: 'TestClassInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassInstVarClass'
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