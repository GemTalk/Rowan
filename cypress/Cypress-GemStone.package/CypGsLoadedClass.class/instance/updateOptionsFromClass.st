"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateOptionsFromClass
	"Options are GemStone-specific. If the class has no options, the property is absent.
	Otherwise, the property is a sorted array of strings. The valid values are:

	dbTransient
	disallowGciStore
	instancesInvariant
	instancesNonPersistent
	modifiable
	subclassesDisallowed
	traverseByCallback

	Not all combinations are legal, but we don't have to worry about that here, since we're getting the values
	from the image, and the image is trusted to only have valid values."

	| propertyName oldValue newValue changed |
	propertyName := '_gs_options'.
	oldValue := self propertiesAt: propertyName.
	newValue := (handle _optionsArray collect: [:option | option asString])
				asSortedCollection.
	newValue isEmpty ifTrue: [newValue := self absentToken].

	"Since the absent token is not equal to any other valid value, we can skip the identity check in this case."
	oldValue = newValue
		ifFalse: 
			[self markPackageDirty.
			newValue == self absentToken
				ifTrue: [self removeProperty: propertyName]
				ifFalse: [self propertiesAt: propertyName put: newValue]]