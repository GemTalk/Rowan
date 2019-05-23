private
loadedMethodAt: aSelector isMeta: isMeta

	^ isMeta
		ifTrue: [ loadedClassMethods at: aSelector ifAbsent: [nil]]
		ifFalse: [ loadedInstanceMethods at: aSelector ifAbsent: [nil] ]