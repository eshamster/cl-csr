FROM eshamster/cl-base:2.4.2

ADD ./test.sh /root
RUN ros install ccl-bin && \
    ros install rove && \
    ros install ps-experiment && \
    ros install cl-ps-ecs

CMD ["/root/test.sh"]
