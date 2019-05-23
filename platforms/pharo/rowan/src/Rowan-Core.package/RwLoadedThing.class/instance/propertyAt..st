accessing
propertyAt: propertyName

	^properties at: propertyName ifAbsent: [AbsentToken]