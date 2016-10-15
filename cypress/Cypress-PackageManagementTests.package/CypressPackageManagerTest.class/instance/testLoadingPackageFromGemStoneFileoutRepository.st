"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests - GemStone fileouts - Future
"
testLoadingPackageFromGemStoneFileoutRepository
	"Presently, filing out .gs files is an option in the repository properties file,
	 and there is no filing in support at all - as a 'repository'.
	 I am considering having a separate repository format, but if one wants to
	 have .gs files in the same directory as the Cypress files, it cannot use a
	 properties file (it would conflict with the other).
	 Additionally, it needs separate files for separate SymbolDictionaries,
	 as well as some notion of the appropriate user."