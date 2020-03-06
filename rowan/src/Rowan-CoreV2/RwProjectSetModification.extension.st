Extension { #name : 'RwProjectSetModification' }

{ #category : '*rowan-corev2' }
RwProjectSetModification >> componentsWithDoits [
	| componentsWithDoits |
	componentsWithDoits := Set new.
	self elementsModified
		keysAndValuesDo: [ :projectName :projectModification | 
			componentsWithDoits
				addAll: projectModification componentsWithDoits ].
	^ componentsWithDoits
]
