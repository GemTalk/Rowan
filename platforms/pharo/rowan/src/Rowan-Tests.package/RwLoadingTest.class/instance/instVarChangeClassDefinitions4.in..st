support - instVarChanges definitions
instVarChangeClassDefinitions4: packageName in: classDefinitionDictionary
	"Inst Var Changes: add instVar3 and remove instVar1"

	^ classDefinitionDictionary
		at: 'TestInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestInstVarClass'
								super: 'Object'
								instvars:
									{'instVar2'.
									'instVar3'}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself