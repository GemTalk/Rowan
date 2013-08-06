accessing
poolDictionaryNames

	^self properties at: 'pools' ifAbsent: [#()]