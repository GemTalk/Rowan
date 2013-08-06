accessing
packageExtensionOr: aBlock

	^self properties at: 'extension' ifAbsent: aBlock
