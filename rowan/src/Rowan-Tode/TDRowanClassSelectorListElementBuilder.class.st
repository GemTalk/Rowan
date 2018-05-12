Class {
	#name : 'TDRowanClassSelectorListElementBuilder',
	#superclass : 'TDClassicClassSelectorListElementBuilder',
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
TDRowanClassSelectorListElementBuilder >> clientList: listElement [

	| items ar beh selectors appEnv |
	appEnv := listElement topez applicationEnvironment.
	items := OrderedCollection new.
	ar := OrderedCollection new.
	beh := self theBehavior.
	selectors := self object protocol = TDProtocolDefinition allSelectorsProtocol
		ifTrue: [ appEnv selectorsFor: beh ]
		ifFalse: [ appEnv selectorsFor: beh in: self object protocol ].
	selectors sorted
		do: [ :selector | 
			| methodDef |
			methodDef := (self methodDefinitionFor: selector)
				appEnv: appEnv;
				yourself.
			ar add: (Text string: selector asString attributes: methodDef emphasis).
			items
				add:
					(TDClientListItemAttributes
						object: methodDef
						parent: nil
						aspect: listElement aspect) ].
	listElement ~~ nil
		ifTrue: [ listElement itemCache: items ].
	^ ar asArray -> nil
]

{ #category : 'accessing' }
TDRowanClassSelectorListElementBuilder >> methodDefinitionFor: selector [

	^ self object asTDClassDefinition
		methodDefinitionFor: selector
		isMeta: self isMeta
]
