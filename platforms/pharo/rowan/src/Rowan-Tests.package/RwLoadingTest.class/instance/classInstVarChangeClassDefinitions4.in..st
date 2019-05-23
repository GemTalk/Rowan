support - classInstVarChanges definitions
classInstVarChangeClassDefinitions4: packageName in: classDefinitionDictionary
	"Class Inst Var Changes: add classInstVar3 and remove classInstVar1"

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
									{'classInstVar2'.
									'classInstVar3'}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself