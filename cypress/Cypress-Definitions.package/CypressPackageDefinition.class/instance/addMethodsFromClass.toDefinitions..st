"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: snapshotting
"
addMethodsFromClass: aClass toDefinitions: definitions
	"Add only those methods which are not extensions from other packages."

	(((aClass methodDictForEnv: 0)
		reject: [:each | (each inClass categoryOfSelector: each selector) first = $*])
			asSortedCollection: [:a :b | a selector <= b selector])
			do: [:method | definitions add: (CypressMethodDefinition forMethod: method)]
