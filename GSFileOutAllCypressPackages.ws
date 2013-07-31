| path packageNames packages fileOuts |
path := '/opt/git/CypressReferenceImplementation'.
packageNames := #('Cypress-Definitions' 'Cypress-Mocks' 'Cypress-Structure' 'Cypress-Tests' 'Cypress-GemStoneFileServer' 'Cypress-Comparison').

packages := packageNames collect: 
				[:each |
				CypressPackageStructure
					fromPackage: (CypressPackageDefinition named: each)].
packages
	do: [:each | CypressPackageWriter writePackageStructure: each to: path].

fileOuts := packages
			inject: Dictionary new
			into: 
				[:result :each |
				result
					at: (each name copyWithoutSuffix: '.package')
						put: (String streamContents: [:stream | each fileOutOn: stream]);
					yourself].
fileOuts keysAndValuesDo: 
		[:packageName :fileOut |
		(GsFile openWriteOnServer: path , packageName , '.gs')
			nextPutAll: fileOut;
			close].
