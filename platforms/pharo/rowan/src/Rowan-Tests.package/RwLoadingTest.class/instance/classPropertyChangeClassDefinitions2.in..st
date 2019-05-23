support - classProperyChange definitions
classPropertyChangeClassDefinitions2: packageName in: classDefinitionDictionary
	"Class Property Changes that do not result in new class versions "

	^ classDefinitionDictionary
		at: 'TestAddClassVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestAddClassVarClass'
								super: 'Object'
								instvars: {}
								classinstvars: {}
								classvars: {'ClassVar1'}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		at: 'TestChangeCommentClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestChangeCommentClass'
								super: 'Object'
								instvars: {}
								classinstvars: {}
								classvars: {}
								comment: 'a new comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself