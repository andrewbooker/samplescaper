synths=(ada pascal go haskell rust)
for s in ${synths[@]}; do
    echo $s
    cd $s
    ./setup.sh
    ./compile.sh
    cd -
done
