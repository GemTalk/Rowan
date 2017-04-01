"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: actions
"
createClassFor: aPatchSet

	| classProperties packageName infoSource type category oldVersion superclass createdClass |
	classProperties := classDefinition properties.
	packageName := packageDefinition key.
	infoSource := Cypress configuration packageInfoSource.
	type := classDefinition classType.
	category := infoSource == #Category
				ifTrue: [packageName]
				ifFalse: [classProperties at: 'category' ifAbsent: ['']].
	oldVersion := self oldClassVersion.
	superclass := aPatchSet superclassNamed: classProperties superclassName.
	createdClass := type = 'normal'
				ifTrue: 
					[superclass
						subclass: classDefinition key
						instVarNames: (classDefinition propertyAt: 'instvars')
						classVars: (classDefinition propertyAt: 'classvars')
						classInstVars: (classDefinition propertyAt: 'classinstvars')
						poolDictionaries: (classDefinition propertyAt: 'pools')
						inDictionary: nil
						newVersionOf: oldVersion
						description: (classDefinition propertyAt: 'comment' ifAbsent: [''])
						options: (classDefinition propertyAt: 'gs_options' ifAbsent: [#()])]
				ifFalse: 
					[type = 'variable'
						ifTrue: 
							[superclass
								indexableSubclass: classDefinition key
								instVarNames: (classDefinition propertyAt: 'instvars')
								classVars: (classDefinition propertyAt: 'classvars')
								classInstVars: (classDefinition propertyAt: 'classinstvars')
								poolDictionaries: (classDefinition propertyAt: 'pools')
								inDictionary: nil
								newVersionOf: oldVersion
								description: (classDefinition propertyAt: 'comment' ifAbsent: [''])
								options: (classDefinition propertyAt: 'gs_options' ifAbsent: [#()])]
						ifFalse: 
							[type = 'bytes'
								ifTrue: 
									[(classDefinition propertyAt: 'instvars') isEmpty
										ifFalse: [self error: 'Cannot define byte class with named instvars.'].
									superclass
										byteSubclass: classDefinition key
										classVars: (classDefinition propertyAt: 'classvars')
										classInstVars: (classDefinition propertyAt: 'classinstvars')
										poolDictionaries: (classDefinition propertyAt: 'pools')
										inDictionary: nil
										newVersionOf: oldVersion
										description: (classDefinition propertyAt: 'comment' ifAbsent: [''])
										options: (classDefinition propertyAt: 'gs_options' ifAbsent: [#()])]
								ifFalse: [self error: 'Unknown class type']]].
	aPatchSet createdClass: createdClass.
	^createdClass