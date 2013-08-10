testing
testVersionShouldParseComplexName
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Seaside2.8b5'
			parsesToPackageName: 'Seaside2'
			author: ''
			branch: '8b5'
			version: 0;
		assert: 'Seaside2.8b5-avi.1'
			parsesToPackageName: 'Seaside2'
			author: 'avi'
			branch: '8b5'
			version: 1;
		assert: 'Seaside-Core-pmm.2'
			parsesToPackageName: 'Seaside-Core'
			author: 'pmm'
			branch: ''
			version: 2;
		assert: 'Seaside-Core.configcleanup-jf.3'
			parsesToPackageName: 'Seaside-Core'
			author: 'jf'
			branch: 'configcleanup'
			version: 3;
		assert: 'Seaside-Core.configcleanup.extraspeedup-lr.69'
			parsesToPackageName: 'Seaside-Core'
			author: 'lr'
			branch: 'configcleanup.extraspeedup'
			version: 69;
		assert: 'Seaside-Core.configcleanup42.extraspeedup-lr.69'
			parsesToPackageName: 'Seaside-Core'
			author: 'lr'
			branch: 'configcleanup42.extraspeedup'
			version: 69
