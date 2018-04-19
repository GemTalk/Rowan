	"Cypress packager - write Cypress as a Rowan Package "
	| repo |
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: 'tonel:$ROWAN_HOME/platforms/gemstone/src/cypress/')
		alias: ''.
	#('Cypress-Comparison' 'Cypress-Definitions' 'Cypress-Environment-Tools' 'Cypress-GemStoneFileServer' 'Cypress-MessageDigest' 'Cypress-Network-Url' 'Cypress-PackageManagement' 'Cypress-Structure')
		do: [ :packageName | 
			| package |
			package := (CypressPackageDefinition named: packageName)
				yourself.
			repo writePackageStructure: (CypressPackageStructure fromPackage: package) ].
