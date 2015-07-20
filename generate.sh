#!/bin/bash

# for each file in data, run line below

if ! grep -q 'deb http://cran.ma.imperial.ac.uk/bin/linux/ubuntu trusty/' /etc/apt/sources.list
then
   	echo "deb http://cran.ma.imperial.ac.uk/bin/linux/ubuntu trusty/" | sudo tee -a /etc/apt/sources.list  > /dev/null
   	sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 > /dev/null
	sudo apt-get update  > /dev/null
	sudo apt-get -y install R-base  > /dev/null
	sudo R CMD javareconf > /dev/null
fi

if !  hash R 2>/dev/null
then
	sudo apt-get -y install R-base > /dev/null
fi

sudo Rscript house_hold.R

#&> /dev/null

