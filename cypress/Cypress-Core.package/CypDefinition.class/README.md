Abstract.
Defines some "code" entity.
The definition object's primary responsibility is to hold information.
It holds exactly the same information as the disk representation in some form of Cypress repository.
Other non-definition classes construct and use the information in definitions to read, write, and compare
code.

properties is a dictionary. Keys and values should be strings. These correspond to the properties in a Cypress repository, and are used in various ways, some of them dialect-specific.