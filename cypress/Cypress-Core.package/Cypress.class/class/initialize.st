"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
initialize
	"Cypress initialize."

	knownRepositories := Set new.
	loadedPackages := Set new.
	configuration := CypConfiguration default.
	platform := CypPlatform current.
	configuration packageByCategory
		ifTrue: [CypLoadedPackage updateAllFromCategories]