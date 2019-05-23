accessing
loadedConfigurationNames

	imageSpec ifNil: [ ^ #() ].
	^ self imageSpec loadedConfigurationNames
