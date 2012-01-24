#/bin/sh

if [ $# -lt 1 ];
then
    echo "Not enough arguments."
    exit 1
fi

THING=$1

if [ -f ~/.kde/share/icons/OS-L/32x32/actions/${THING}.png ];
then
    convert -composite template.png ~/.kde/share/icons/OS-L/32x32/actions/${THING}.png ${THING}.xpm
else
    echo "Original icon not found."
    exit 1
fi


