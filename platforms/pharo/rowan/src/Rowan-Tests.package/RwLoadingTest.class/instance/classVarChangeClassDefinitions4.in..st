support - classVarChanges definitions
classVarChangeClassDefinitions4: packageName in: classDefinitionDictionary
	"Class Var Changes: add ClassVar3 and remove ClassVar1"

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
									{'ClassVar2'.
									'ClassVar3'}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself