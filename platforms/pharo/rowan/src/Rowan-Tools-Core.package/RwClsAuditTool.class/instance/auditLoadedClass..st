audit
auditLoadedClass: aLoadedClass
"look for methods compiled into class without Rowan API"
| res  |

	res := self _result.
	(Rowan globalNamed: aLoadedClass name)  
		ifNil: [self errorLog: res add: aLoadedClass name -> 'Missing gemstone class for loaded class ' ] "there is no matching Class for LoadedClass"
		ifNotNil: [:aBehavior | 
			"audit class properties"
			self errorLog: res addAll:  (self _auditLoadedClassProperties: aLoadedClass forBehavior: aBehavior).
			"audit categories"

			aBehavior categorysDo: [:category :selectors | 
				self errorLog: res addAll: (self  _auditCategory: category forBehavior: aBehavior loadedClass: aLoadedClass)
			].
			aBehavior class categorysDo: [:category :selectors | 
				self errorLog: res  addAll: (self  _auditCategory: category forBehavior: aBehavior class loadedClass: aLoadedClass)
			].
		"audit loaded class methods"
		aLoadedClass 
			loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :aLoadedMethod | (aBehavior compiledMethodAt: aLoadedMethod name otherwise: nil) 
					ifNil: [(self _auditLoadedInstanceMethod: aLoadedMethod forBehavior: aBehavior loadedClass: loadedClass) ifNotNil: [:a | self errorLog: res add: a]]]

			loadedClassMethodsDo: [:loadedProject :loadedPackage :loadedClass :aLoadedMethod |(aBehavior class compiledMethodAt: aLoadedMethod name otherwise: nil) 
					ifNil: [(self _auditLoadedClassMethod: aLoadedMethod forBehavior: aBehavior class loadedClass: loadedClass)  ifNotNil: [:a | self errorLog: res add: a]]
			]
	].
	^res