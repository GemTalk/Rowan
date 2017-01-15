"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updatePoolDictionaryNamesFromClass
	"Order of pool dictionaries *does* matter in GemStone, so we keep the names in the order given in the image."

	| propertyName oldNames newNames |
	propertyName := 'pools'.
	oldNames := self propertyAt: propertyName.
	newNames := handle sharedPools collect: [:each | each name asString].
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]