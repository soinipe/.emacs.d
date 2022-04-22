:
# Create directory with sudo

#set -x

user=${USER}
group=$(id -gn)

files=( "$@" )

for file in "${files[@]}"
do
    echo Creating ${file}...
    sudo mkdir ${file}
    sudo chown ${user} ${file}
    sudo chgrp ${group} ${file}
done

# End of file
