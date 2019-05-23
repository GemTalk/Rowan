other
auditLoadedClassExtension: aLoadedClassExtension
"look for methods compiled into class without Rowan API"
| res   extensionCategoryName|

	res := self _result.
	extensionCategoryName :=  aLoadedClassExtension loadedPackage asExtensionName "'*' , aLoadedClassExtension loadedPackage name" .
	(Rowan globalNamed: aLoadedClassExtension name) 
		ifNil: [self errorLog: res  add: aLoadedClassExtension name -> ' Class does not exists for loaded class '] 
		ifNotNil: [:aBehavior ||categories | 
					
			categories := (aBehavior _baseCategorys: 0)
				ifNil: [ #() ]
				ifNotNil: [:catDict | catDict keys ].
			(categories	
				detect: [:each | each equalsNoCase: extensionCategoryName ] ifNone: [ ])
					ifNotNil: [:aCategory | self errorLog: res  addAll:  (self _auditCategory: aCategory forBehavior: aBehavior loadedClass: aLoadedClassExtension)]
					ifNil: [aLoadedClassExtension loadedInstanceMethods notEmpty ifTrue: [
						self errorLog: res add: aLoadedClassExtension name , ' #' ,extensionCategoryName -> 'Missing instance method extension category ' ] ].

			categories := (aBehavior class _baseCategorys: 0)
				ifNil: [ #() ]
				ifNotNil: [:catDict | catDict keys ].
			(categories
				detect: [:each | each equalsNoCase: extensionCategoryName ] ifNone: [ ])
					ifNotNil: [:aCategory | self errorLog: res  addAll:  (self _auditCategory: aCategory forBehavior: aBehavior class loadedClass: aLoadedClassExtension)]
					ifNil: [aLoadedClassExtension loadedClassMethods notEmpty ifTrue: [
						self errorLog: res add: aLoadedClassExtension name , ' #' ,extensionCategoryName -> 'Missing class method extension category ' ] ].

			aLoadedClassExtension 
				loadedInstanceMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod | 
					(self _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aBehavior loadedClass: loadedClass) ifNotNil: [:x | self errorLog: res add: x]
				] 
				loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |
					(self _auditLoadedClassMethod: aLoadedMethod forBehavior: aBehavior class loadedClass: loadedClass) ifNotNil: [:x |self errorLog: res add: x]
				]
		].
		^res