# LazPackageEmbeddedAVR
This is a project wizard as Lazarus IDE plugin for creating empty projects for Target embedded: AVR.
The device specific project settings are generated, a programmer command is optional for direct flashing of the device on key press from within the Lazarus IDE.
As special feature a device specific interrupt file is generated for easiest use of interrupts.

## Installation:
Download all files as zip. Unpack the zip to a random place. You will have to keep the files at that place after installation. Open the Lazarus IDE, and install the package. Therefore click menue "Package" -> "Open Package File (.lpk)".
Choose "lazpackageembeddedavr.lpk", the package dialog opens. Press "Use" -> "Install". Confirm to rebuild the Lazarus IDE.

LazPackageEmbeddedAVR doesn't do any compiler or programmer installation. You have to do it seperately. An easy way to install the proper FreePascal compiler is using FPCUpDeluxe. Also take care that your programmer software is installed properly and accessible by command line (usally it means it's added to the PATH environment variable, or using symlinks under Linux). 

## Usage:
After installation there will be a new item in the Lazarus dialog for new projects. The Item is called "AVR Embedded Project".


## Requirements:
At least Lazarus 2.0.0
