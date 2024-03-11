#!/usr/bin/env bash
#gradle build
#gradle jar
gradle run

echo > res.txt
cd outRust
mkdir outExe
cd outExe
for i in {0..9}
do
  rustc ../file$i/file$i.rs
done

cd ../..
for i in {0..9}
do
  timeout 5 ./outRust/outExe/file$i >> res.txt
done
