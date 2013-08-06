"
notice: This work is protected by copyright. All rights reserved.
category: initialization
"
fromPackage: aCypressPackageDefinition

	| snapshot classMap classDefinitions classStructure |
	snapshot := aCypressPackageDefinition snapshot.
	name := aCypressPackageDefinition name, self packageExtension.
	properties := Dictionary new.
	classDefinitions := OrderedCollection new.
	classMap := Dictionary new.
	snapshot definitions do: [:definition |  
			definition 
				classDefinition: [:classDefinition |  classDefinitions add: classDefinition ] 
				methodDefinition: [:methodDefinition | 
					(classMap 
						at: methodDefinition className 
						ifAbsentPut: [Set new]) 
							add: methodDefinition. ]].
	classDefinitions do: [:classDefinition |
		classStructure := (CypressClassStructure fromClassDefinition: classDefinition)
			packageStructure: self.
		(classMap removeKey: classDefinition className ifAbsent: [#()]) do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self classes add: classStructure ].
	classMap keysAndValuesDo: [:className :methods |
		classStructure := (CypressClassStructure named: className)
			isClassExtension: true;
			packageStructure: self.
		methods do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self extensions add: classStructure ].
