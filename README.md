## Format 3d images

This is code to take the images from a Stereo Camera (e.g. a weeview 3d
camera or a StereoPi V2 camera), crop them and format them for use in
Sebastes stereo-image analysis software.

To install the package use the lines below. The function requires the
“RSQLite” and “magick” packages that can be downloaded and installed
from the CRAN repository. The package also requires the installation of
the ROpenCVLite and Rvision packages. The installation instructions can
be found here <https://github.com/swarm-lab/ROpenCVLite> and here
<https://github.com/swarm-lab/Rvision>. Installing all these things
requires the installation of the CRAN package “devtools”.

    install.packages("devtools")
    devtools::install_github("rooperc4/SebastesImageFormat")
    library(SebastesImageFormat)

The package has a single function that can be run using the following
code. The function prompts the user to point to a folder where the
stereo camera photos are located that need to be converted. Then it will
print the list of images in that folder. Next it will prompt the user
for a location to place the converted images and the metadata required
by Sebastes. It will then do the image conversion and produce the
required metadata. It is important to note that the metadata generated
by this function is not informative about the images. The function can
also be run from the command line using the second example where the
from and to file folders are specified. There is limited help
documentation as well.

    #Example 1: User is prompted for file locations
    ImageFormat()

    #Example 2: Command line specification of file locations
    ImageFormat("D:/Weeview/PHOTO","C:/Users/rooperc/Desktop/WeeviewTest")
