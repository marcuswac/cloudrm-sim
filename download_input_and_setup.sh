wget --no-check-certificate -r 'https://docs.google.com/uc?export=download&id=1wmp3rMwjEoP1u2V08TupeaZcy3dgf4Ur' -O cloudrm-input-data-google.tar.gz
tar xvzf cloudrm-input-data-google.tar.gz && rm cloudrm-input-data-google.tar.gz
mkdir output 2> /dev/null
mkdir plots 2> /dev/null
Rscript install_R_packages.R
