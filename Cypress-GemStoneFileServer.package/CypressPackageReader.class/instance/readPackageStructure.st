reading
readPackageStructure

   packageStructure := CypressPackageStructure named: (self fileUtils localNameFrom: self packageDirectory).
   (self fileUtils directoryEntriesFrom: self packageDirectory)
        do: [ :entry | 
		(entry endsWith: '/properties.json')
			ifTrue: [ self packageStructure properties: (self readPropertiesFile: entry) ].
            (entry endsWith: '.class')
                ifTrue: [ self packageStructure classes add: (self readClassStructureFromEntry: entry) ].
            (entry endsWith: '.extension')
                ifTrue: [ self packageStructure extensions add: (self readExtensionClassStructureFromEntry: entry) ] ]