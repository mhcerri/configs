if [ "$_id" = "debian" ] || [ "$_id" = "ubuntu" ]; then
    export DEBFULLNAME="Marcelo Henrique Cerri"
    export DEBEMAIL="mhcerri@gmail.com"
    export LC_ALL=C.UTF-8
    alias fdr='fakeroot debian/rules'
    alias dquilt="quilt --quiltrc=${HOME}/.dquilt.rc"
    complete -F _quilt_completion $_quilt_complete_opt dquilt
fi
