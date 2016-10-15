"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testEmptyString
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: ''
			parsesToPackageName: ''
			author: ''
			branch: ''
			version: 0
