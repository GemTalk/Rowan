"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testVersionAlone
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '..1'
			parsesToPackageName: ''
			author: ''
			branch: '.1'
			version: 0;
		assert: '.-.1'
			parsesToPackageName: ''
			author: ''
			branch: ''
			version: 1
