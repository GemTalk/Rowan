	"read in filetree format"
   | packageManager repo |
    packageManager := CypressPackageManager3 new.
    repo := CypressAbstractRepository
      onUrl: (CypressUrl absoluteFromText: 'cypressft:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/ston/tests_filetree/'  )
      alias: ''.
    packageManager
      defaultSymbolDictionaryName: #Globals.
    #('STON-Tests' 'STON-GemStone-Tests' )
      do: [ :packageName | 
        packageManager
          addResolvedReference:
            (CypressResolvedReference name: packageName repository: repo) ].
    packageManager loadResolvedReferences.

	"write in tonel format"
	|  repo url |
	url := CypressUrl absoluteFromText: 'tonel:$ROWAN_PROJECTS_HOME/Rowan/platforms/gemstone/projects/ston/src/'.
	repo := CypressAbstractRepository
		onUrl: url
		alias: ''.
	#('STON-Tests' 'STON-GemStone-Tests' )
		do: [ :packageName | 
			| package |
			package := (CypressPackageDefinition named: packageName)
				yourself.
			repo writePackageStructure: (CypressPackageStructure fromPackage: package) ].

