instance creation
withAll: somePackageRefs

	^(self new)
		initialize;
		addAll: somePackageRefs;
		yourself