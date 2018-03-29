echo "#include <cstddef>" > $2
echo "namespace $3 {" >> $2
echo "unsigned char cl_source[] = {" >> $2
hexdump -v -e '"" /1 "0x%02x" ",\n"' $1 >> $2
echo "0x00 };" >> $2
echo "size_t cl_source_size = sizeof(cl_source)-1;" >> $2
echo "}" >> $2
