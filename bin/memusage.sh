:
#
# Print memory usage of processes.
#
# Adapted from https://stackoverflow.com/questions/131303/how-can-i-measure-the-actual-memory-usage-of-an-application-or-process,
# specifically https://stackoverflow.com/a/44711589/7168565.
#

# 'tail' removes the header from 'ps' command
ps -eo size,pid,user,command --sort +size | \
    tail -n +2 | \
    awk '{ hr=$1/1024 ; printf("%13.2f Mb ",hr) } { for ( x=4 ; x<=NF ; x++ ) { printf("%s ",$x) } print "" }' |\
    cut -d "" -f2 | cut -d "-" -f1

# End of file
