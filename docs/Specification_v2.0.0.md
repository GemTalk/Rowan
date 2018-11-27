# Rowan v2.0.0
## Summary
## Introduction
## Rowan Specification
### Defining an image
1. A Smalltalk image is composed of a collection of configurations.
2. A configuration is a named collection of configurations and packages.
3. A package is a named collection of class definitions, class extensions and method definitions. 
4. A class definition defines the attributes of a class. The class so defined is considered to be defined in the package. A class definition includes a list of class and instance methods for the class, that are also considered to be defined in the package.
4. A class extension defines a list of class and instance side methods for a class that is not defined in the package.
5. A method definition defines the source code and protocol of a method.
3. A configuration is loaded from a project.
7. A kernel configuration represents the collection of classes and methods that are part of the image provided by a vendor. A kernel configuration is versioned by the image vendor. Different versions of the kernel may be loaded into the image as long as the primitive set is compatible.
8. The version of the kernel configuration is used for conditionally including packages in a loaded configuration.
6. A top-level configuration may be loaded into the image by Rowan. A top-level configuration represents a complete unit of functionality that depends upon the classes and methods defined in the kernel configuration.
7. A kernel configurations is a kind of top-level configuration.
7. A nested configuration may not be directly loaded into the image by Rowan. A nested configuration is referenced directly or indiretly from a loadable, top-level configuration.
8. A nested configuration represents a pluggable unit of functionality that depends upon classes and methods defined via the top-level configuration that include it.
8. When a configuration is loaded, the commit id of the project from which it is loaded (SHA in git) is recorded with the configuration.



