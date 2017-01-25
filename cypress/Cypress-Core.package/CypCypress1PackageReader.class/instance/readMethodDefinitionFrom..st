"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
readMethodDefinitionFrom: aStream

	| properties notice category source selector |
	properties := Dictionary new.
	(aStream peekFor: $")
		ifTrue: [aStream nextLine]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(aStream match: 'notice: ')
		ifTrue: [properties at: 'notice' put: aStream nextLine trimSeparators]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(aStream match: 'category: ')
		ifTrue: [properties at: 'protocol' put: aStream nextLine trimSeparators]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(aStream peekFor: $")
		ifTrue: [aStream nextLine]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	source := aStream upToEnd.
	selector := Cypress platform parseSelectorFrom: source.
	properties at: 'selector' put: selector.
	^CypMethodDefinition withProperties: properties source: source