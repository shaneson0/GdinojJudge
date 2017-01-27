

#create user and homedir
sudo  /usr/sbin/useradd -m -u 1536 judge



#create work dir 

sudo    mkdir /home/judge/etc
sudo    mkdir /home/judge/data
sudo    mkdir /home/judge/log
sudo    mkdir /home/judge/run0
sudo    mkdir /home/judge/run1
sudo    mkdir /home/judge/run2
sudo    mkdir /home/judge/run3
sudo	mkdir /home/judge/temp


# add quan xian 
sudo chown -R root /home/judge/data
sudo chown -R root /home/judge/log /home/judge/etc /home/judge/run?
sudo chmod 711 /home/judge /home/judge/data
sudo chgrp judge /home/judge/run?
sudo chmod 771 /home/judge/run?
sudo chmod 771 /home/judge/temp




