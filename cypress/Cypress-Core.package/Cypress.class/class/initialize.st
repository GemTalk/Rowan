"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
initialize
	"Cypress initialize."

	knownRepositories := Set new.
	loadedPackages := Dictionary new.
	platform := CypPlatform current.
	configuration := platform defaultConfiguration