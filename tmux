#!/bin/bash
tmux new -s sicp -n racket 'cd /data/repos/dshs/course-sicp/exercises; fish' \; splitw -h -p 45 -t 1 -c '/data/repos/dshs/course-sicp/exercises' 'racket -i -l xrepl -l sicp-rkt' \; selectp -t 1 \; attach
