Programmer targets are added by adding a new line in the definition files AVRDude.dat or Others.dat. The first string in quotation marks is the name of the programmer displayed in the pulldown menu. The second string in quotation marks is the command line for the programmer. Placeholders are as follows:

Identifier // Description // Example
---------------------------------------------
$(LAZWIZDevice) // device name // atmega32
$(LAZWIZProject) // project name // Project1
$(LAZWIZPort) // programmer port // COM2
