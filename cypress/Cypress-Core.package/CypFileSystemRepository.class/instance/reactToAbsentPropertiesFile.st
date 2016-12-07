"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
reactToAbsentPropertiesFile
	"Default right now is to error if the properties file is absent. Subclasses can override this.
	This might be something to make configurable via Cypress configuration."

	^self error: 'Properties file not found.'