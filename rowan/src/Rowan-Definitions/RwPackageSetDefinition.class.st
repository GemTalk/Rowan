"
No class-specific documentation for CypPackageSetDefinition, hierarchy is: 
Object
  CypDefinition( properties)
    CypPackageSetDefinition( packages)

"
Class {
	#name : 'RwPackageSetDefinition',
	#superclass : 'RwDefinitionSetDefinition',
	#category : 'Rowan-Definitions'
}

{ #category : 'accessing' }
RwPackageSetDefinition >> addPackage: aDefinition [

	self addDefinition: aDefinition
]

{ #category : 'deriving' }
RwPackageSetDefinition >> deriveLoadedThings [

	^ RwEntitySet
		withAll:
			((self definitionNames
				collect: [ :packageName | Rowan image loadedPackageNamed: packageName ifAbsent: [  ] ])
				select: [ :each | each notNil ])

]

{ #category : 'accessing' }
RwPackageSetDefinition >> packageNames [

	^ self definitionNames
]

{ #category : 'accessing' }
RwPackageSetDefinition >> packages [

	^ self definitions
]
