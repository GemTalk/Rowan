"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
forCypress

	^(self new)
		comparingPackages: #('Cypress-Definitions' 'Cypress-Mocks' 'Cypress-Structure' 'Cypress-Tests' 'Cypress-GemStoneFileServer' 'Cypress-Comparison')
			fromDirectory: '/opt/git/CypressReferenceImplementation/';
		yourself
