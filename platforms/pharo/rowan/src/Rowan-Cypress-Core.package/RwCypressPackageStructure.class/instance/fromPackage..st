initialization
fromPackage: aRwPackageDefinition

	| classStructure |
	aRwPackageDefinition classDefinitions
		do: [ :classDefinition | 
			classStructure := (CypressClassStructure fromClassDefinition: classDefinition)
				packageStructure: self.
			classDefinition instanceMethodDefinitions
				do: [ :methodDefinition | 
					| methodStructure |
					methodStructure := RwCypressMethodStructure new
						fromRwInstanceMethodDefinition: methodDefinition;
						packageStructure: self;
						classStructure: classStructure.
					classStructure instanceMethods
						at: methodDefinition selector
						put: methodStructure ].
			classDefinition classMethodDefinitions
				do: [ :methodDefinition | 
					| methodStructure |
					methodStructure := RwCypressMethodStructure new
						fromRwClassMethodDefinition: methodDefinition;
						packageStructure: self;
						classStructure: classStructure.
					classStructure classMethods
						at: methodDefinition selector
						put: methodStructure ].
			self classes add: classStructure ].
	aRwPackageDefinition classExtensions
		do: [ :extensionDefinition | 
			classStructure := (CypressClassStructure named: extensionDefinition name)
				isClassExtension: true;
				packageStructure: self.
			extensionDefinition instanceMethodDefinitions
				do: [ :methodDefinition | 
					| methodStructure |
					methodStructure := RwCypressMethodStructure new
						fromRwInstanceMethodDefinition: methodDefinition;
						packageStructure: self;
						classStructure: classStructure.
					classStructure instanceMethods
						at: methodDefinition selector
						put: methodStructure ].
			extensionDefinition classMethodDefinitions
				do: [ :methodDefinition | 
					| methodStructure |
					methodStructure := RwCypressMethodStructure new
						fromRwClassMethodDefinition: methodDefinition;
						packageStructure: self;
						classStructure: classStructure.
					classStructure classMethods
						at: methodDefinition selector
						put: methodStructure ].
			self extensions add: classStructure ].
	name := aRwPackageDefinition key , self packageExtension.
	properties := Dictionary new
