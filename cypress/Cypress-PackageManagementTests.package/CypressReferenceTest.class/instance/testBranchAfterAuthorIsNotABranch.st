"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testBranchAfterAuthorIsNotABranch

	self
		assert: 'Seaside-Core-jf.configcleanup.3'
			parsesToPackageName: 'Seaside-Core'
			author: 'jf.configcleanup'
			branch: ''
			version: 3;
		assert: 'Seaside-Core-lr.configcleanup.extraspeedup.69'
			parsesToPackageName: 'Seaside-Core'
			author: 'lr.configcleanup.extraspeedup'
			branch: ''
			version: 69;
		assert: 'Seaside-Core-lr.configcleanup42.extraspeedup.69'
			parsesToPackageName: 'Seaside-Core'
			author: 'lr.configcleanup42.extraspeedup'
			branch: ''
			version: 69
