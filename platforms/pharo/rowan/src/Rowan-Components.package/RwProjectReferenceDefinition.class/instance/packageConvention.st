accessing
packageConvention

	^ self properties at: 'packageConvention' ifAbsent: [ 'RowanHybrid' ]