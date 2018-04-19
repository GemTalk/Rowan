	"STON packager - write STON as a Rowan Package "
	| repo |
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: 'cypress:$ROWAN_HOME/platforms/gemstone/src/ston/')
		alias: ''.
	#('STON-Core' 'STON-GemStoneBase' 'STON-GemStoneCommon')
		do: [ :packageName | 
			| package |
			package := (CypressPackageDefinition named: packageName)
				lookupSymbolList: Rowan image symbolList;
				yourself.
			repo writePackageStructure: (CypressPackageStructure fromPackage: package) ].
