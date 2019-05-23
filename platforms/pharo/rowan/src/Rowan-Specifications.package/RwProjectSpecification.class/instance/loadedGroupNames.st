accessing
loadedGroupNames

	imageSpec ifNil: [ ^ #() ].
	^ self imageSpec loadedGroupNames
