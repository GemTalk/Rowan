accessing
addDefinedGroupName: groupName 

	"add a group that does not include any other groups"

	self addDefinedGroupName: groupName includeGroups: #()
