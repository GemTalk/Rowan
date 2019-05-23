querying
_loadedProjectNamedForNonTestProjects: aString ifAbsent: absentBlock

	"Look up a loaded project. First in current users project registry, then scan AllUsers for the project ... note that the user needs to be the project owner."

	| currentUserId |
	currentUserId := self currentUserId.
	^ (super _loadedProjectRegistryForUserId: currentUserId)
		at: aString
		ifAbsent: [ 
			AllUsers
				do: [ :userProfile | 
					| aUserId |
					aUserId := userProfile userId.
					aUserId ~= currentUserId
						ifTrue: [ 
							(super _loadedProjectRegistryForUserId: aUserId)
								ifNotNil: [ :projectRegistry | 
									(projectRegistry at: aString ifAbsent: [  ])
										ifNotNil: [ :loadedProject | ^ loadedProject ] ] ] ].
			^ absentBlock value ]
