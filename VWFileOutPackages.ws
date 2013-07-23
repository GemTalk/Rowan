"
| path packages fileOuts |
packages := #('Cypress-Definitions' 'Cypress-Mocks' 'Cypress-Structure' 'Cypress-Tests' 'Cypress-GemStoneFileServer').
path := 'C:\Users\rsargent\Documents\VisualWorks Projects\CypressReferenceImplementation\'.
fileOuts := GBSM currentSession
		evaluate: 'CypressPackageDefinition fileOutsForPackagesNamed: self'
		context: packages.
fileOuts keysAndValuesDo: [:packageName :fileOut |
	(Filename named: path, packageName, '.gs') writeStream
		nextPutAll: fileOut;
		close
].
"
"
CypressPackageWriter 
	writePackageStructure: (CypressPackageStructure fromPackage: (CypressPackageDefinition named: 'Cypress-Mocks')) 
	to: '/home/rsargent/cypress/'
"
"
CypressPackageReader 
	readPackageStructureFrom: '/home/rsargent/cypress/Cypress-Mocks.package'
"