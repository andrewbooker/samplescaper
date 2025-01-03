for d in $1/202*/
do
    mv $d*.wav $1/looped
    rm -r $d
done

