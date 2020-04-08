Extension { #name : 'RwProjectModification' }

{ #category : '*rowan-corev2' }
RwProjectModification >> componentsWithDoits [
	^ self after
		ifNotNil: [ :projectDef | projectDef componentsWithDoits ]
]
