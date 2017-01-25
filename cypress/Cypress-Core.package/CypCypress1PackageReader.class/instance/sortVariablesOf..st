"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
sortVariablesOf: properties
	"Properties is a properties dictionary. Class variable names and pool variable names should be sorted, if present."

	#('classvars' 'pools') do: 
			[:propertyName |
			| values |
			values := properties at: propertyName ifAbsent: [nil].
			values
				ifNotNil: [properties at: propertyName put: values asSortedCollection]]