private
_addPackageNames: somePackageNames for: aConfiguration

	super _addPackageNames: somePackageNames for: aConfiguration.
	somePackageNames do: [:aPackageName |
		(packageToConfigurationMap at: aPackageName ifAbsentPut: [ {} ])
			add: aConfiguration ]
