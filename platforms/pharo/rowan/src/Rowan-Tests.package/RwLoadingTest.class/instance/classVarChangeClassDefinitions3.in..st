support - classVarChanges definitions
classVarChangeClassDefinitions3: packageName in: classDefinitionDictionary
	"Class Var Changes: add ClassVar2"

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
								classvars:
									{'ClassVar1'.
									'ClassVar2'}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself