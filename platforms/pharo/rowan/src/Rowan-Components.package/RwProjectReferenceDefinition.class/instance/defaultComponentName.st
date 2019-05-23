accessing
defaultComponentName

	^ self properties at: 'defaultComponentName' ifAbsent: [ 'Core' ]