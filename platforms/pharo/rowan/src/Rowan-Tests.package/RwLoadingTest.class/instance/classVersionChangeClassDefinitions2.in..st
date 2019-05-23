support - classVersionChanges
classVersionChangeClassDefinitions2: packageName in: classDefinitionDictionary
	"Class Property Changes that result in new class versions "

	^ classDefinitionDictionary
		at: 'TestAddInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestAddInstVarClass'
								super: 'Object'
								instvars: {'instVar1'}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		at: 'TestAddClassInstVarClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestAddClassInstVarClass'
								super: 'Object'
								instvars: {}
								classinstvars: {'classInstVar1'}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
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
		at: 'TestChangeSuperclassClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestChangeSuperclassClass'
								super: 'Association'
								instvars: {}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		at: 'TestChangePoolsClass'
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: 'TestChangePoolsClass'
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