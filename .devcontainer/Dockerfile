FROM docker.io/erlang:25.0

RUN apt-get update; apt-get install -y sudo
RUN useradd -s /bin/bash -m vscode; echo 'vscode ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

USER vscode

RUN tmp=$(mktemp); \
    (echo '$include /etc/inputrc' && tail -n+3 /etc/inputrc) > $tmp; \
    mv $tmp ~/.inputrc; \
    for line in $(grep -n history-search ~/.inputrc | cut -d':' -f1); do \
        sed -E "${line}s|^#||g" -i ~/.inputrc; \
    done; 

USER root

RUN apt-get install -y less vim

RUN mkdir /usr/local/lib/erlang/lib/.vscode && \
    echo '{"files.exclude": {"**/*.beam": true,"**/*.chunk": true}}' > /usr/local/lib/erlang/lib/.vscode/settings.json && \
    chown -R vscode:vscode /usr/local/lib/erlang/lib/.vscode

CMD [ "/sbin/init" ]
