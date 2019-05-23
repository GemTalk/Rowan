merging
mergeForNewClassVersionWith: aMethodModification

	"validate that this is a proper modification"

	self sourceModification isEmpty
		ifTrue: [ 
			aMethodModification sourceModification isEmpty
				ifTrue: [ 
					self
						error:
							'internal error - unexpected missing source modification during method modification for new class version merge' ]
				ifFalse: [ 
					"this is as it should be ... method source must be carried forward to the new class version"
					 ] ].
	self propertiesModification isEmpty
		ifTrue: [ 
			aMethodModification propertiesModification isEmpty
				ifFalse: [ self error: 'internal error - not yet implemented' ] ]
