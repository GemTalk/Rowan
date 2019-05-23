private
removeKey: key from: aDictionary

	aDictionary
		removeKey: key
		ifAbsent: [ self error: 'no definition for ' , key printString , ' found' ]