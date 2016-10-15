"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
testPackageAndAuthorOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements-dhk'
			parsesToPackageName: 'Announcements'
			author: 'dhk'
			branch: ''
			version: 0
