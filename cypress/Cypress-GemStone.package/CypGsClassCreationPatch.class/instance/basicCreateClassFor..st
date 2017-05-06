"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
basicCreateClassFor: aPatchSet

	| type oldVersion superclass createdClass |
	type := classDefinition classType.
	oldVersion := self oldClassVersion.
	superclass := aPatchSet superclassNamed: classDefinition superclassName.
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
	^createdClass