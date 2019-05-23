support - classInstVarChanges definitions
classInstVarChangeClassDefinitions3: packageName in: classDefinitionDictionary
	"Class Inst Var Changes: add classInstVar2"

	^ classDefinitionDictionary
		at: 'TestClassInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestClassInstVarClass'
								super: 'Object'
								instvars: {}
								classinstvars:
									{'classInstVar1'.
									'classInstVar2'}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself