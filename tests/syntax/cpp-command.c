/* run.config*
   FILTER: sed 's|/tmp/[^ ]*\.i|/tmp/FILE.i|g'
   OPT: -no-autoload-plugins -cpp-frama-c-compliant -cpp-command "echo ['%1' '%1' '%i' '%input'] ['%2' '%2' '%o' '%output'] ['%args']"
   OPT: -no-autoload-plugins -cpp-frama-c-compliant -cpp-command "echo %%1 = '%1' %%2 = '%2' %%args = '%args'"
   OPT: -no-autoload-plugins -cpp-frama-c-compliant -cpp-command "printf "%s" 'using \\% has no effect : \%input'"
   OPT: -no-autoload-plugins -cpp-frama-c-compliant -cpp-command "echo %var is not an interpreted placeholder"
   */