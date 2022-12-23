Extension { #name : 'SystemResolver' }

{ #category : '*rowan-components-kernel' }
SystemResolver >> rowanProjectsHome [

	"Answer the path to $ROWAN_PROJECTS_HOME"

	^ (System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME')
		ifNil: [ self error: '$ROWAN_PROJECTS_HOME not defined' ]
		ifNotNil: [:str | self  resolveString: str ]
]
