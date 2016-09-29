# EVEN SLACKWARE! (EXAMPLE)

FROM vbatts/slackware

ADD https://s3.amazonaws.com/download.fpcomplete.com/pid1/pid1-0.1.0-amd64 /sbin/pid1
RUN chmod 755 /sbin/pid1
ENTRYPOINT [ "/sbin/pid1" ]

CMD bash -c 'while(true); do sleep 1; echo alive; done'
