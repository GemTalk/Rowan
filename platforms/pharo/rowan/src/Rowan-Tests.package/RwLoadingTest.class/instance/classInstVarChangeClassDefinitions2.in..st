support - classInstVarChanges definitions
classInstVarChangeClassDefinitions2: packageName in: classDefinitionDictionary
	"Class Inst Var Changes: add classInstVar1"

	^ classDefinitionDictionary
		at: 'TestClassInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassInstVarClass'
								super: 'Object'
								instvars: {}
								classinstvars: {'classInstVar1'}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself