"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testPackageAlone
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: ''
			version: 0;
		assert: 'Announcements.-.'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: ''
			version: 0
