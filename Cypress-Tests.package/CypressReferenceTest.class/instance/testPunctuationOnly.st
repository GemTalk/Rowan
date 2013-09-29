"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testPunctuationOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.-.'
			parsesToPackageName: ''
			author: ''
			branch: ''
			version: 0;
		assert: '.-'
			parsesToPackageName: ''
			author: ''
			branch: ''
			version: 0;
		assert: '.'
			parsesToPackageName: ''
			author: ''
			branch: ''
			version: 0;
		assert: '..'
			parsesToPackageName: ''
			author: ''
			branch: '.'
			version: 0
