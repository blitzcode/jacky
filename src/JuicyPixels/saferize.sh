
# UNIX Shell Version of saferizer.ps1 from the JuicyPixels repository

find . -name "*.hs" \
    -exec sed -i -e 's:\.unsafeWrite:.write:g'   {} +
find . -name "*.hs" \
    -exec sed -i -e 's:\.unsafeRead:.read:g'     {} +
find . -name "*.hs" \
    -exec sed -i -e 's:`V.unsafeIndex`:V.!:g'    {} +
find . -name "*.hs" \
    -exec sed -i -e 's:`VS\.unsafeIndex`:VS.!:g' {} +
find . -name "*.hs" \
    -exec sed -i -e 's:BU.unsafeIndex:B.index:g' {} +
find . -name "*.hs" \
    -exec sed -i -e 's:V.unsafeIndex:(V.!):g'    {} +
find . -name "*.hs" \
    -exec sed -i -e 's:VS.unsafeIndex:(VS.!):g'  {} +
find . -name "*.hs" \
    -exec sed -i -e 's:unsafeIndex:!:g'          {} +
find . -name "*.hs" \
    -exec sed -i -e 's:unsafeFreeze:freeze:g'    {} +
   

