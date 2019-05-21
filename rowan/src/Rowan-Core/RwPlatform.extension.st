Extension { #name : 'RwPlatform' }

{ #category : '*rowan-core' }
RwPlatform >> projectNamed: aName [

	"Answer a project with the given name.  If no project with the given name is found, signals error."

	| project |
	project := RwProject newNamed: aName.
	project _loadedProject.	"signal error, if the project does not exist"
	^ project
]

{ #category : '*rowan-core' }
RwPlatform >> projectNamed: aName ifPresent: presentBlock ifAbsent: absentBlock [

	"Lookup a loaded project with the given name, if found evaluate the <presentBlock>, if not evaluate the <absentBlock.."

	^ Rowan image
		loadedProjectNamed: aName
		ifPresent: [:loadedProject |  presentBlock cull: (RwProject newNamed: aName) ]
		ifAbsent: absentBlock
]
