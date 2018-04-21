	"Tonel packager - write Tonel as a Rowan Package "
	| repo |
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: 'tonel:$ROWAN_HOME/platforms/gemstone/projects/tonel/src/')
		alias: ''.
	#('CypressTonel-Core' 'CypressTonel-GemStone-Tests' 'CypressTonel-Tests'
		'Tonel-Core' 'Tonel-Cypress-Tests' 'Tonel-GemStone-Tests' 'Tonel-GemStoneCommon-Core'
		'Tonel-Tests')
		do: [ :packageName | 
			| package |
			package := (CypressPackageDefinition named: packageName)
				yourself.
			repo writePackageStructure: (CypressPackageStructure fromPackage: package) ].
