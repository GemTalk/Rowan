"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
updateLoadedPackages
	"Update all LoadedPackages and their contents, based on the configured packageInfoSource."

	| source |
	source := configuration packageInfoSource.
	"If the source is Cypress, no updating is needed -- the LoadedPackages *are* the authoritative source"
	source == #Cypress ifFalse: [platform updateLoadedPackagesFrom: source]