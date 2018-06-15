%.lc: %.lam
	echo $^
	gcc -E -x c -I. -P $^ | awk -f rename.awk > $@

%.blc: %.lc
	cat ~/build/binary-lambda-calculus/ioccc/parse.Blc $^ | ~/build/binary-lambda-calculus/uni -o > $@

%.Blc: %.blc
	cat ~/build/binary-lambda-calculus/ioccc/deflate.Blc $^ | ~/build/binary-lambda-calculus/uni -o > $@
