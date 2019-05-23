class writing
_classExtensionOf: aClassExtension
	"order is alphabetical"

	| definition |
	definition := self class orderedDictionaryClass new.

	definition at: #name put: aClassExtension name.

	^ self _toJSON: definition