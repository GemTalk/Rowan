	"Cypress packager - write Cypress as a Rowan Package "
	| repo |
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: 'tonel:$ROWAN_HOME/platforms/gemstone/projects/cypress/src/')
		alias: ''.
	#('Cypress-Comparison' 'Cypress-Definitions' 'Cypress-Environmental-Tools' 'Cypress-GemStoneFileServer' 'Cypress-MesssageDigest' 'Cypress-Network-Url' 'Cypress-PackageManagement' 'Cypress-Structure')
		do: [ :packageName | 
			| package |
			package := (CypressPackageDefinition named: packageName)
				yourself.
			repo writePackageStructure: (CypressPackageStructure fromPackage: package) ].
