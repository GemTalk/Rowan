# The Rowan Specification
Rowan is a [declarative][1], cross dialect, project management system for Smalltalk.

## Differences between Rowan and Monticello/Metacello

### Declarative Object Model
The Rowan object model is a superset of the [Monticello][2] object model.
In addition to the standard Monticello definitions:
- class definitions
- method definitions
Rowan includes definitions for projects, packages and class extensions.

A class extension is an explicit list of method definitions that extend a class.

A package definition is a named collection of class definitions and class extension definitions.

A project definition is a named collection of project definitions.

### Metacello/Monticello Loading Model: package-centric and imperative
Monticello is a package-based system and the package is the smallest loadable unit.
However, the package is not the smallest functional unit.

Because of conditional loading requirements (cross platform and cross version code changes) a single package does not always represent an independent chunk of functionality. 

In order to make it feasible to load a single package and meet the requirement that such loads would result in an independent chunk of functionality, [Metacello][3] uses package dependencies to ensure that no matter which package was loaded, the developer would get a complete unit of functionality.

However in practice the package dependencies defined in a Metacello baseline tend to be biased towards resolving load issues like package load order and making sure that classes/globals are defined before they are used rather than biased towards defining functional units.
For example, the following (imperative) Metacello load expression is likely to load without error:
```smalltalk
Metacello new
 baseline:'Seaside3';
 repository: 'github://SeasideSt/Seaside:master/repository';
 load: #('Seaside-Core')
```
however it is not likely that you will end up with a cogent, functional unit.
In Metacello functional units are defined by groups like the following (imperative) Metacello load expression:
```smalltalk
Metacello new
 baseline:'Seaside3';
 repository: 'github://SeasideSt/Seaside:master/repository';
 load: #('Core')
```
where the Core goup is expected to be a standalone functional unit.

### Rowan Loading Model: configuration-centric and declarative
#### Rowan Configurations
A Rowan Configuration is a named list of packages that are loaded together as a unit.
Here is an example of a simple Rowan Configuration that loads the **Core** functionality of the project:
```ston
RwProjectCompoundConfiguration{
	#name : 'Core',
	#packageNames : [
		'RowanSample1-Core',
		'RowanSample1-Extensions'
	],
	#comment : 'Platform independent packages, that should always be loaded together.'
}
```
A Rowan Configuration may reference another Rowan Configuration.
Here the **Test** configuration requires the **Core** configuration:
```ston
RwProjectCompoundConfiguration{
	#name : 'Test',
	#packageNames : [
		'RowanSample1-Tests'
	],
	#configurationNames : [ 
		'Core'
	],
	#comment : 'Platform independent Test packages. The Test configuration requires the Core configuration.'
}
```
Configuration references may be recursive.

Rowan Configurations may define *groups* and specify confitional loading for packages:
```ston
RwProjectLoadConfiguration{
	#name : 'Globals',
	#definedGroupNames : {
		'core' : [ ]
		},
	#conditionalPackages : {
		[ 'common' ] : {
    	'core': {
				#packageNames : [
					'Rowan-Cypress-Kernel',
					'Rowan-Tools-Kernel'
					]
				}
			},
		[ 'gemstone' ] : {
      'core': {
				#packageNames : [
					'Rowan-GemStone-Kernel'
					]
				}
			},
		[ 'gs3.[2-]' ] : {
      'core': {
				#packageNames : [
					'GemStone-Interactions-Kernel'
					]
				}
			},
		[ 'gs3.2.[15-]' ] : {
      'core': {
				#packageNames : [
					'Rowan-GemStone-3215'
					]
				}
			}
		},
	#comment : 'Rowan Globals configuration - packages are loaded into Globals symbol dictionary'
}
```
Since Rowan Configurations are instance-based, the configuration model may be extended by introducing new Configuration classes.
 
#### Rowan Declarative Loading
The initial load of a Rowan projet uses a Rowan Load Specification to specify the load parameters which include:
- list of configurations to load
- list of groups to load
- cloning from remote project
   - location of remote project.
   - location to clone 
- attaching to a local disk-based project
   - path to project.

[1]: http://www.smalltalksystems.com/publications/_awss97/SSDCL1.HTM
[2]: http://www.wiresong.ca/monticello/
[3]: https://github.com/Metacello/metacello

