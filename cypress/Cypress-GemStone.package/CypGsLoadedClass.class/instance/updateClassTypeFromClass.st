"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateClassTypeFromClass
	"For GemStone, the valid values are:
	bytes
	indexable
	immediate
	nsc
	No property is recorded for non-indexable pointer objects."

	| propertyName oldValue newValue changed |
	propertyName := '_gs_type'.
	oldValue := self propertyAt: propertyName.
	newValue := handle isBytesOrSpecial
				ifTrue: [handle isBytes ifTrue: ['bytes'] ifFalse: ['immediate']]
				ifFalse: 
					[handle isNsc
						ifTrue: ['nsc']
						ifFalse: 
							[handle isVariable
								ifTrue: ['indexable']
								ifFalse: [self absentToken	"non-indexable pointer object"]]].

	"Since the absent token is not equal to any other valid value, we can skip the identity check in this case."
	oldValue = newValue
		ifFalse: 
			[newValue == self absentToken
				ifTrue: [self removeProperty: propertyName]
				ifFalse: [self propertyAt: propertyName put: newValue]]