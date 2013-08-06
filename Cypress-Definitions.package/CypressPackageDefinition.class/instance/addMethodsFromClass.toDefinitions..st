"
notice: This work is protected by copyright. All rights reserved.
category: snapshotting
"
addMethodsFromClass: aClass toDefinitions: definitions
	"Add only those methods which are not extensions from other packages."

	((aClass methodDictionary reject: [:each | each category first = $*])
		asSortedCollection: [:a :b | a selector <= b selector])
			do: [:method | definitions add: method asCypressMethodDefinition]