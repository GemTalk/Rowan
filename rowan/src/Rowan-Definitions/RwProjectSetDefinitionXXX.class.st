Class {
	#name : 'RwProjectSetDefinitionXXX',
	#superclass : 'RwProjectSetDefinition',
	#category : 'Rowan-Definitions'
}

{ #category : 'deriving' }
RwProjectSetDefinitionXXX >> deriveLoadedThings [

	"extract the loaded projects that correspond to the project defintions held by the receiver"

	^ RwEntitySetXXX
		withAll:
			((self definitionNames
				collect: [ :projectName | Rowan image loadedProjectNamed: projectName ifAbsent: [  ] ])
				select: [ :each | each notNil ])
]
