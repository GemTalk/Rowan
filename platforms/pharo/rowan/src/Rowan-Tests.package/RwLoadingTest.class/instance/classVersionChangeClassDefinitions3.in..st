support - classVersionChanges
classVersionChangeClassDefinitions3: packageName in: classDefinitionDictionary
	"Class Property Changes that result in new class versions "

	"add methods that reference the new class properties"

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
						instanceMethodDefinitions:
							(self classVersionChangeInstVarMethodsForClass3: 'TestAddInstVarClass'));
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
						classMethodDefinitions:
							(self classVersionChangeClassInstVarMethodsForClass3: 'TestAddInstVarClass')
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
						classMethodDefinitions:
							(self classVersionChangeClassVarMethodsForClass3: 'TestAddClassVarClass')
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
						instanceMethodDefinitions:
							(self
								classVersionChangeAssociationInstanceMethodsForClass3:
									'TestChangeSuperclassClass'));
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