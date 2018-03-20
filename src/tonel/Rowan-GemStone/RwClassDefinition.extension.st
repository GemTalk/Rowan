Extension { #name : 'RwClassDefinition' }

{ #category : '*rowan-gemstone' }
RwClassDefinition >> _modificationForcingNewClassVersion [

	^ RwClassModificationForcingNewClassVersion
]

{ #category : '*rowan-gemstone' }
RwClassDefinition >> gs_options: optionsArray [

	(optionsArray _isArray and: [ optionsArray isEmpty not ])
		ifTrue: [ 
			self properties
				at: 'gs_options'
				put:
					(optionsArray collect: [ :each | each asString ]) asSortedCollection asArray ]
]
