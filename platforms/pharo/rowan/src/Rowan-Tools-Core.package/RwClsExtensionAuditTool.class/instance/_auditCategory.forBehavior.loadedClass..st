other
_auditCategory: anExtentionCategory forBehavior: aClassOrMeta loadedClass: aLoadedClassExtension
"if we have loaded methods but no compiled methods add error to result"
| res |
	res := super _auditCategory: anExtentionCategory selectors:  (aClassOrMeta selectorsIn: anExtentionCategory) forBehavior: aClassOrMeta loadedClass: aLoadedClassExtension.
	aClassOrMeta isMeta 
		ifTrue: [
			(aLoadedClassExtension loadedClassMethods notEmpty and: [(aClassOrMeta selectorsIn: anExtentionCategory) isEmpty])
				ifTrue: [ self errorLog: res  add: ((aLoadedClassExtension name , ' #' ,anExtentionCategory asString -> 'Missing expected class methods in the category '))   ].
	] 	ifFalse: [
			(aLoadedClassExtension loadedInstanceMethods notEmpty and: [(aClassOrMeta selectorsIn: anExtentionCategory) isEmpty])
				ifTrue: [ self errorLog: res  add: (aLoadedClassExtension name , ' #' ,anExtentionCategory asString -> ('Missing expected instances methods in the category'))   ].
	].
	^res
