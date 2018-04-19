	"STON packager - write STON as a Rowan Package "
	| repo |
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: 'tonel:$ROWAN_HOME/platforms/gemstone/src/ston/')
		alias: ''.
	#('STON-Core' 'STON-GemStoneBase' 'STON-GemStoneCommon')
		do: [ :packageName | 
			| package |
			package := (CypressPackageDefinition named: packageName)
				yourself.
			repo writePackageStructure: (CypressPackageStructure fromPackage: package) ].
