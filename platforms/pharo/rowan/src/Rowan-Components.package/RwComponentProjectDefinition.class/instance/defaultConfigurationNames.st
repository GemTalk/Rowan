properties
defaultConfigurationNames

	^ self propertyAt: 'defaultConfigurationNames' ifAbsent: [ self projectRef configurationNames ]