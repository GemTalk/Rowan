actions
read: platformConfigurationAttributes
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the default component and group names"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ Rowan projectTools read
		readProjectSetForComponentProjectDefinition: self 
			withConfigurations: self defaultConfigurationNames
			groupNames: self defaultGroupNames
			platformConfigurationAttributes: platformConfigurationAttributes