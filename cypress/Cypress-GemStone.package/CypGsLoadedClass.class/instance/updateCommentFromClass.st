"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateCommentFromClass

	| propertyName oldValue newValue absentToken |
	propertyName := 'comment'.
	absentToken := self absentToken.
	oldValue := self propertiesAt: propertyName.
	newValue := (handle _extraDictAt: #comment) ifNil: [absentToken].
	(newValue ~~ absentToken and: [newValue isEmpty])
		ifTrue: [newValue := absentToken].

	"Absent token must be compared by identity, because a comment of '<absent>' would be equal to no comment."
	(oldValue == absentToken | (newValue == absentToken)
		ifTrue: [oldValue == newValue]
		ifFalse: [oldValue = newValue])
			ifFalse: 
				[self markPackageDirty.
				newValue == absentToken
					ifTrue: [self removeProperty: propertyName]
					ifFalse: [self propertiesAt: propertyName put: newValue]]