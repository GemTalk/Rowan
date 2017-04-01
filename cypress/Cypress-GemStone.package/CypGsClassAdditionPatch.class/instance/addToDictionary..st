"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: actions
"
addToDictionary: aDictionary
	"Dictionary is class name -> classAdditionPatch. Error on duplicate name."

	| name |
	name := classDefinition key.
	name ifNil: [self error: 'Class definition with no name.'].
	(aDictionary includesKey: name) ifTrue: [self error: 'Duplicate name'].
	aDictionary at: name put: self