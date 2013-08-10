testing
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
