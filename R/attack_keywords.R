#' @title Attack keywords
#' @export


attack_keywords <- function() {

  # SQLi keywords
  sqlikeywords <- c(
    'select', 'union', 'order', 'where', 'dual',
    'count', 'sleep', 'benchmark', 'delete', 'insert',
    'create', 'alter', 'table', 'drop', 'index',
    'boolean', 'like', 'rlike', 'null', 'information',
    'schema', 'waitfor', 'delay', 'and', 'dbms',
    'chr', 'concat', 'extractvalue', 'char', 'updatexml',
    'boolean', 'group', 'ornot', 'makeset', 'allusers',
    'casewhen', 'userlock', 'generateseries'
  )

  sqliKey <- hashmap::hashmap(
    keys = sqlikeywords,
    values = 's' %>% rep(sqlikeywords %>% length)
  )

  # XSS keywords
  xsskeywords <- c(
    'alert', 'applet', 'base', 'comment', 'embed', 'frame',
    'frameset', 'handler', 'iframe', 'import',
    'isindex', 'link', 'listener', 'meta', 'noscript',
    'object', 'script', 'style', 'vmlframe', 'xml', 'xss',
    'onerror', 'img', 'onload', 'logchr'
  )

  xssKey <- hashmap::hashmap(
    keys = xsskeywords,
    values = 'x' %>% rep(xsskeywords %>% length)
  )

  # Bash keywords
  bashkeywords <- c(
    'alias', 'apropos', 'apt-get', 'aptitude', 'aspell', 'awk',
    'basename', 'bash', 'builtin', 'cfdisk', 'chgrp', 'chmod',
    'chown', 'chroot', 'chkconfig', 'cksum', 'cron','crontab',
    'csplit', 'curl', 'ddrescue', 'diff', 'dircolors', 'dirname',
    'dirs', 'dmesg', 'echo', 'egrep', 'env', 'etc', 'ethtool',
    'eval', 'exec', 'export', 'expr', 'fdformat', 'fdisk',
    'fgrep', 'fmt', 'fsck', 'ftp', 'function', 'fuser', 'gawk',
    'getopts', 'grep', 'groupadd', 'groupdel', 'groupmod', 'gzip',
    'hash', 'hostname', 'htop', 'iconv', 'ifconfig', 'ifdown',
    'ifup', 'install', 'killall', 'locate', 'logname', 'lprint',
    'lprintd', 'lprintq', 'lprm', 'lsblk', 'lsof', 'mkdir',
    'mkfifo', 'mkisofs', 'mknod', 'mtools', 'netstat', 'nohup',
    'nslookup', 'opt', 'passwd', 'ping', 'pgrep', 'pkill', 'popd',
    'printcap', 'printenv', 'printf', 'proc', 'pushd', 'pwd',
    'quotacheck', 'readarray', 'readonly', 'renice', 'remsync',
    'rmdir', 'rsync', 'sbin', 'scp', 'sdiff', 'sed', 'sftp', 'shopt',
    'shutdown', 'slocate', 'ssh', 'strace', 'sudo', 'suspend',
    'sync', 'timeout', 'tmp', 'tput', 'tsort', 'tty', 'type',
    'ulimit', 'umask', 'umount', 'unalias', 'uname', 'unexpand',
    'uniq', 'unrar', 'unshar', 'uptime', 'useradd', 'userdel',
    'usermod', 'usr', 'uuencode', 'uudecode', 'vdir', 'vmstat',
    'whereis', 'whoami', 'wget', 'xargs', 'xdg-open'
  )

  bashKey <- hashmap::hashmap(
    keys = bashkeywords,
    values = 'b' %>% rep(bashkeywords %>% length)
  )

  # Save attack words as a list
  attackWords <<- list(SQLi = sqliKey, XSS = xssKey, BASH = bashKey)
}
