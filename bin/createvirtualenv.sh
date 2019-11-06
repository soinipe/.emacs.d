:
# Helper to install virtualenv&wrapper on Ubuntu16.04

mkdir $HOME/.virtualenvs
cd /tmp
wget https://bootstrap.pypa.io/get-pip.py
sudo python get-pip.py
sudo pip install virtualenv virtualenvwrapper
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh
echo -e "\n# virtualenv and virtualenvwrapper" >> ~/.bashrc
echo "export WORKON_HOME=$HOME/.virtualenvs" >> ~/.bashrc
echo "source /usr/local/bin/virtualenvwrapper.sh" >> ~/.bashrc
source ~/.bashrc
mkvirtualenv p2 -p python2
# For Ubuntu 18.04, workaround for ModuleNotFoundError: No module named 'distutils.spawn'
# sudo apt-get install python3-distutils
mkvirtualenv p3 -p python3

# End of file
