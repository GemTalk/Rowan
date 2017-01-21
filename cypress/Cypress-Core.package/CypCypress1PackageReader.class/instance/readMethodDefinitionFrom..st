"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
readMethodDefinitionFrom: aStream

	| properties notice category source selector |
	self halt: 'Method in progress'.
	(aStream peekFor: $")
		ifTrue: [aStream nextLine]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(aStream match: 'notice: ')
		ifTrue: [notice := aStream nextLine trimSeparators]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(aStream match: 'category: ')
		ifTrue: [category := aStream nextLine trimSeparators]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(aStream peekFor: $")
		ifTrue: [aStream nextLine]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	source := aStream upToEnd.
	selector := Cypress platform parseSelectorFrom: source.
	^CypMethodDefinition withProperties: properties source: source

	"	methods at: selector
		put: ((CypressMethodStructure new)
				packageStructure: self packageStructure;
				classStructure: classStructure;
				name: selector;
				isMetaclass: isMeta;
				selector: selector;
				category: category;
				source: source;
				yourself)"