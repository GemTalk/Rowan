Holds some number of RwDefinitions or RwLoadedThingss to be operated on together. The entities put into the receiver should be at the same semantic level: all methods, class, packages , or projects.

When the receiver is sent #asDefinition, the receiver is converted to a RwDefinitionSetDefinition and all entities are converted to their definition counterparts.