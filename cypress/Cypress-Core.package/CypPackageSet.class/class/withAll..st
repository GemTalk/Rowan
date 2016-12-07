"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
withAll: somePackageRefs

	^(self new)
		initialize;
		addAll: somePackageRefs;
		yourself