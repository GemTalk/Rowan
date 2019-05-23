audit
_auditCategory: category selectors: aSelectorSet forBehavior: aBehavior loadedClass: aLoadedClass
	|  res |
	
		res := res := self _result.

		aSelectorSet do: [:aSelector |
				(
					aBehavior isMeta
						ifTrue: [	self  _auditClassSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass ]
						ifFalse: [ self  _auditSelector: aSelector forBehavior: aBehavior loadedClass: aLoadedClass]	
				)  ifNotNil: [:aRes | self errorLog: res add: aRes]
				
		].

		^res
