support - classVarChanges definitions
classVarChangeClassDefinitions2: packageName in: classDefinitionDictionary
	"Class Var Changes: add ClassVar1"

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
								classvars: {'ClassVar1'}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself