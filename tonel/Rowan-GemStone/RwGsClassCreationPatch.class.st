"
No class-specific documentation for CypGsClassCreationPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassPatch( classDefinition packageDefinition)
      CypGsClassCreationPatch

"
Class {
	#name : 'RwGsClassCreationPatch',
	#superclass : 'RwGsClassPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'private' }
RwGsClassCreationPatch >> basicCreateClassFor: aPatchSet [
	"For GemStone, the valid values are:
	bytes
	variable
	immediate
	normal for non-indexable pointer object"

	"Must be in-synch with RwGsLoadedClass>>updateClassTypeFromClass"

	| type oldVersion superclass createdClass gs_options |
	type := classDefinition classType.
	oldVersion := self oldClassVersion.
	superclass := aPatchSet superclassNamed: classDefinition superclassName.
	gs_options := ((classDefinition propertyAt: 'gs_options' ifAbsent: [#()]) collect: [ :each | each asSymbol ]).
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
						options: gs_options ]
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
								options: gs_options ]
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
										options: gs_options ]
								ifFalse: [self error: 'Unknown class type']]].
	^createdClass
]

{ #category : 'actions' }
RwGsClassCreationPatch >> createClassFor: aPatchSet [

	| createdClass |
	createdClass := self privateCreateClassFor: aPatchSet.
	aPatchSet createdClass: createdClass.
	^createdClass
]

{ #category : 'private' }
RwGsClassCreationPatch >> privateCreateClassFor: aPatchSet [

	| classProperties packageName infoSource category createdClass |
	createdClass := self basicCreateClassFor: aPatchSet.
	classProperties := classDefinition properties.
	packageName := packageDefinition key.
	infoSource := Rowan configuration packageInfoSource.
	category := infoSource == #Category
				ifTrue: [packageName]
				ifFalse: [classProperties at: 'category' ifAbsent: ['']].
	createdClass category: category.
	^createdClass
]
