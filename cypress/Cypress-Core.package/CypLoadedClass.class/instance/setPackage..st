"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
setPackage: aLoadedPackage

	package ifNotNil: [self error: 'Package already set'].
	package := aLoadedPackage