FROM eshamster/cl-base:2.4.2

ADD ./test.sh /root
RUN ros install ccl-bin && \
    ros install rove && \
    ros install eshamster/ps-experiment && \
    ros install eshamster/cl-ps-ecs

CMD ["/root/test.sh"]
