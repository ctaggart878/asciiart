# JPEG to ascii art
# Copyright (c) 2014, Daniel Baamonde for original matlab code, his license is included below.
# Copyright (c) 2014, C. Taggart Grant (Do whatever you want with my part, except break the law 
# or be mean to people, or violate Baamonde's license.)

# Pass in a URL for a jpeg/jpg image, and get back ascii art.
# If an image is gigantic, try scaling it down more. 

asciiArt <- function(myUrl, scaleDown = 1) { 
	require('jpeg')
	require('png')
	scaleDown <- scaleDown 
	
	# Check what file type we're looking at. 
	# Thank you Adrie on stack overflow:  http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
	substrRight <- function(x, n){
	  substr(x, nchar(x)-n+1, nchar(x))
	}

	fileType <- substrRight(myUrl, 3)
		
	# This is going to drop a file into your R working directory. 
	# There are other ways to do this, but this was easy. 
	# If you wonder where it is, just use getwd() to see where it ended up. 
		
	download.file(myUrl, "WhoPutThisShitOnMyComputer")
	
	if (fileType == 'png') { 
		img <- readPNG("WhoPutThisShitOnMyComputer")
	} else {
		img <- readJPEG("WhoPutThisShitOnMyComputer")
	}
	# Let's check if we're in color.  If so, convert to gray scale.
	# We'll use a luminosity algorithm to conver to gray scale
	# Thanks John D. Cook:  http://www.johndcook.com/blog/2009/08/24/algorithms-convert-color-grayscale/

	if (length(dim(img))==3) {
		lum <- c(.21, .71, .07)
		img[,,1] <- img[,,1] * lum[1]
		img[,,2] <- img[,,2] * lum[2]
		img[,,3] <- img[,,3] * lum[3]
		grayImg <- img[,,1] + img[,,2] + img[,,3]	

	} else {
		grayImg <- img
	}


	# Let's set up our ascii box.  We maybe will make this more dynamic later. 
	# Get our current dimensions
	grayRows <- dim(grayImg)[1]
	grayCols <- dim(grayImg)[2]

	# Let's set up our ascii box, which is scaled down
	# %/% is integer division in R
	asciiRows <- grayRows %/% (7 * scaleDown)
	asciiCols <- grayCols %/% (4 * scaleDown)
	myBox <- matrix(data = NA, nrow = asciiRows, ncol = asciiCols)

	# Let's make our original image fit our new box by clipping off the extra. 
	extraRows <- grayRows %% (7 * scaleDown)
	extraCols <- grayRows %% (4 * scaleDown)
	grayImgAdj <- grayImg[1:(grayRows-extraRows),1:(grayCols-extraCols)]

	# Update our rows for the trimmed image. 
	grayRows <- grayRows - extraRows
	grayCols <- grayCols - extraCols

	# Scale our gray image down by taking an average of the pixel
	# values that are being packed into a single ascii character

	smallerGrayImg <- matrix(NA,asciiRows,asciiCols)

	# Shit just got real. We double-nested-looped in R. Sorry. 
	# Also, we could just treat this data as a vector and then 
	# reshape back it into a matrix later, but let's get something
	# that works first. 
	for (i in 1:asciiRows) { 
		for (j in 1:asciiCols) {
			largeImgRowsRange <- ((i-1) * (7 * scaleDown)) + seq(1,(7 * scaleDown))
			largeImgColsRange <- ((j-1) * (4 * scaleDown)) + seq(1,(4 * scaleDown))
			smallerGrayImg[i,j] <- mean(grayImg[largeImgRowsRange,largeImgColsRange])
		}
	}

	# Some adjustmenst to the range of gray we've got in our new image. 
	imgMin <- min(min(smallerGrayImg))
	smallerGrayImg <- smallerGrayImg - imgMin
	imgMax <- max(max(smallerGrayImg))
	smallerGrayImg <- smallerGrayImg / imgMax

	# We're just going to use this character set to do the drawings.
	asciis <- rev(unlist(strsplit(' .,:;irsXA253hMHGS#9B&@', "")))

	# This is gives a numerical vector 23 element in length, so that
	# we can map a certain greyscale value to our characters.
	# This also replaces linspace from matlab.
	map <- seq(0,1,length = 24)
	mappedGrayImg <- cut(smallerGrayImg, map, labels = asciis)
	asciiImg <- matrix(mappedGrayImg,asciiRows,asciiCols,byrow=FALSE)


	for (i in 1:asciiRows) {
		for (j in 1:asciiCols) {
			cat(asciiImg[i,j])
		}
		cat('\n')
	}

	asciiImg
}






# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the distribution
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
