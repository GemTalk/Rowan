accessing
addDefinedGroupName: groupName includeGroups: includedGroups

	"add a group that does not include any other groups"

	self definedGroupNames at: groupName put: includedGroups asArray sort
